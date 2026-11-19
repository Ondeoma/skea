package skea.api

import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skea.core.*
import skea.shared.*

import java.util.UUID

/**
 * エージェント専用アカウント: 管理者だけが画面から作り・キーを発行・失効でき、キーはそのアカウントの権限で動く。ログインはできない。
 * DB を使う（ApiDbSuite）。
 */
class AgentAccountSpec extends ApiDbSuite {

  private def keyReq(scopes: List[String]): Json = CreateApiKeyReq("skakapo", scopes, None, Some(30)).asJson

  withEnv("管理者がエージェント専用アカウントを作り、そのキーで MCP・REST を使える。失効・無効化で使えなくなり、ログインはできない") { env =>
    for {
      a <- env.user("管理 花子")
      (adm, at) = a
      _ <- env.users.update(adm.id, Some(true), None)
      n <- env.user("一般 太郎")
      (usr, nt) = n
      // 管理者でなければ 403
      _ <- env.get(nt, "/api/admin/agents").map(r => assertEquals(r._1, Status.Forbidden))
      _ <- env.post(nt, "/api/admin/agents", CreateAgentReq("x").asJson).map(r => assertEquals(r._1, Status.Forbidden))
      // 名前の確認
      _ <-
        env.post(at, "/api/admin/agents", CreateAgentReq("  ").asJson).map(r => assertEquals(r._1, Status.BadRequest))
      _ <- env.post(at, "/api/admin/agents", CreateAgentReq("a\u0007b").asJson).map(r =>
        assertEquals(r._1, Status.BadRequest)
      )
      created <- env.post(at, "/api/admin/agents", CreateAgentReq(" skakapo エージェント ").asJson)
      _ = assertEquals(created._1, Status.Created)
      agentId = created._2.hcursor.get[UUID]("id").toOption.get
      _ = assertEquals(created._2.hcursor.get[String]("name").toOption, Some("skakapo エージェント"))
      // admin スコープは付けられない
      _ <- env.post(at, s"/api/admin/agents/$agentId/keys", keyReq(List("read", "write", "admin"))).map(r =>
        assertEquals(r._1, Status.BadRequest)
      )
      // ほかのユーザーにはキーを発行できない
      _ <- env.post(at, s"/api/admin/agents/${usr.id}/keys", keyReq(List("read"))).map(r =>
        assertEquals(r._1, Status.NotFound)
      )
      k1 <- env.post(at, s"/api/admin/agents/$agentId/keys", keyReq(List("read", "write")))
      _ = assertEquals(k1._1, Status.Created)
      key1 = k1._2.hcursor.get[String]("key").toOption.get
      // 一覧にキーが出る（ハッシュや平文は出さない）
      list <- env.get(at, "/api/admin/agents")
      mine = list._2.as[List[AgentAccountDto]].toOption.get.find(_.id == agentId).get
      _ = assertEquals(mine.keys.map(k => (k.scopes, k.revoked)), List((List("read", "write"), false)))
      _ = assert(!list._2.noSpaces.contains(key1) && !list._2.noSpaces.toLowerCase.contains("hash"))
      // キーで MCP（tools/list）
      mcp <- env.callKey(
        key1,
        Method.POST,
        "/mcp",
        Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "tools/list".asJson))
      )
      _ = assertEquals(mcp._1, Status.Ok)
      _ = assert(mcp._2.hcursor.downField("result").downField("tools").values.exists(_.nonEmpty), mcp._2)
      // キーでは管理できない（admin・画面からのみ）、自分のキーも発行できない
      _ <- env.callKey(key1, Method.GET, "/api/admin/agents").map(r => assertEquals(r._1, Status.Forbidden))
      _ <- env.callKey(key1, Method.POST, "/api/me/api-keys", Some(keyReq(List("read")))).map(r =>
        assertEquals(r._1, Status.Forbidden)
      )
      // 管理者のキー（admin スコープ）でもエージェントは管理できない（画面からのみ）
      ak <- env.post(at, "/api/me/api-keys", keyReq(List("read", "write", "admin")))
      _ <- env.callKey(ak._2.hcursor.get[String]("key").toOption.get, Method.GET, "/api/admin/agents").map(r =>
        assertEquals(r._1, Status.Forbidden)
      )
      // 権限は普通のユーザーと同じ: メンバーにするまでプロジェクトは見えず、編集権限を付けると書き込める
      pkey = "AG" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(6)
      pid <- env.projects.create(pkey, "エージェントの試験", "", adm.id).map(_.toOption.get)
      _ <- env.callKey(key1, Method.POST, s"/api/projects/$pid/tasks", Some(CreateTaskReq("AI が作ったタスク").asJson)).map(
        r => assertEquals(r._1, Status.NotFound)
      )
      _ <- env.put(at, s"/api/projects/$pid/members", SetMemberReq(agentId, "edit").asJson).map(r =>
        assertEquals(r._1, Status.NoContent)
      )
      task <- env.callKey(key1, Method.POST, s"/api/projects/$pid/tasks", Some(CreateTaskReq("AI が作ったタスク").asJson))
      _ = assertEquals(task._1, Status.Created, task._2)
      _ <- env.callKey(key1, Method.GET, s"/api/projects/$pid").map(r => assertEquals(r._1, Status.Ok))
      // 失効させたキーは使えない（ほかのユーザーのキーは失効できない）
      k2 <- env.post(at, s"/api/admin/agents/$agentId/keys", keyReq(List("read")))
      key2 = k2._2.hcursor.get[String]("key").toOption.get
      key2Id = k2._2.hcursor.downField("info").get[UUID]("id").toOption.get
      _ <- env.callKey(key2, Method.GET, "/api/me").map(r => assertEquals(r._1, Status.Ok))
      _ <- env.call(at, Method.DELETE, s"/api/admin/agents/${usr.id}/keys/$key2Id").map(r =>
        assertEquals(r._1, Status.NotFound)
      )
      _ <- env.call(at, Method.DELETE, s"/api/admin/agents/$agentId/keys/$key2Id").map(r =>
        assertEquals(r._1, Status.NoContent)
      )
      _ <- env.callKey(key2, Method.GET, "/api/me").map(r => assertEquals(r._1, Status.Unauthorized))
      // 無効にするとキーも使えない。戻せば使える。名前も変えられる
      _ <- env.call(at, Method.PATCH, s"/api/admin/agents/$agentId", Some(UpdateAgentReq(None, Some(true)).asJson)).map(
        r => assertEquals(r._1, Status.NoContent)
      )
      _ <- env.callKey(key1, Method.GET, "/api/me").map(r => assertEquals(r._1, Status.Unauthorized))
      _ <- env.post(at, s"/api/admin/agents/$agentId/keys", keyReq(List("read"))).map(r =>
        assertEquals(r._1, Status.BadRequest)
      )
      _ <- env.call(
        at,
        Method.PATCH,
        s"/api/admin/agents/$agentId",
        Some(UpdateAgentReq(Some("skakapo"), Some(false)).asJson)
      ).map(r => assertEquals(r._1, Status.NoContent))
      me <- env.callKey(key1, Method.GET, "/api/me")
      _ = assertEquals((me._1, me._2.hcursor.get[String]("name").toOption), (Status.Ok, Some("skakapo")))
      // エージェント専用アカウントでないユーザーは変えられない
      _ <-
        env.call(at, Method.PATCH, s"/api/admin/agents/${usr.id}", Some(UpdateAgentReq(None, Some(true)).asJson)).map(
          r => assertEquals(r._1, Status.NotFound)
        )
      // ログインできない（同じ sub でのログインは拒否し、名前・メールも書き換わらない）
      agent <- env.users.find(agentId).map(_.get)
      _ = assert(AgentAccounts.isAgent(agent.sub) && agent.email.isEmpty, agent)
      login <- env.users.upsert(agent.sub, "乗っ取り", Some("evil@example.com"), None).attempt
      _ = assert(login.isLeft)
      after <- env.users.find(agentId).map(_.get)
      _ = assertEquals((after.name, after.email), ("skakapo", None))
    } yield ()
  }
}
