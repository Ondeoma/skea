package skea.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import skea.auth.SecretBox
import skea.core.*
import skea.db.*
import skea.shared.*

import java.util.UUID

/** プロジェクトの通知を stui のチャンネルに送る（Webhook の kind = stui）。stui は偽物（送られた要求を記録する）。 */
class StuiChannelSpec extends ApiDbSuite:

  private val box = new SecretBox("test-secret-0123456789")

  /** 偽の stui: チャンネルの一覧と、通知の受け取り（notifyStatus を返す）。 */
  private final class FakeStui(notifyStatus: Ref[IO, Status], got: Ref[IO, List[Json]]):
    val client: Client[IO] = Client.fromHttpApp(HttpRoutes.of[IO] {
      case GET -> Root / "api" / "channels" =>
        Ok(Json.arr(
          Json.obj("id" -> "c-dev".asJson, "name" -> "dev".asJson, "kind" -> "public".asJson, "archived" -> false.asJson, "member" -> true.asJson,
            "role" -> "admin".asJson),
          Json.obj("id" -> "c-ops".asJson, "name" -> "ops".asJson, "kind" -> "private".asJson, "archived" -> false.asJson),
          Json.obj("id" -> "c-old".asJson, "name" -> "old".asJson, "kind" -> "public".asJson, "archived" -> true.asJson),
          Json.obj("id" -> "c-dm".asJson, "name" -> "dm".asJson, "kind" -> "dm".asJson, "archived" -> false.asJson)))
      case req @ POST -> Root / "api" / "apps" / "notify" =>
        req.as[Json].flatMap(j => got.update(_ :+ j)) *> notifyStatus.get.map(Response[IO](_))
      // 受信 Webhook の作成: 管理者のチャンネル（c-dev）だけ作れる. URL は利用者向けの公開 URL で返す
      case req @ POST -> Root / "api" / "hooks" / "incoming" =>
        req.as[Json].flatMap { j =>
          if j.hcursor.get[String]("channelId").toOption.contains("c-dev") then
            got.update(_ :+ Json.obj("created" -> j)) *> Created(Json.obj("hook" -> Json.obj("id" -> "h-1".asJson), "url" -> "https://chat.example.com/hooks/tok_123".asJson))
          else Forbidden(Json.obj("error" -> "管理者だけ".asJson))
        }
      case DELETE -> Root / "api" / "hooks" / "incoming" / id => got.update(_ :+ Json.obj("deleted" -> id.asJson)) *> NoContent()
      case req @ POST -> Root / "hooks" / "tok_123" => req.as[Json].flatMap(j => got.update(_ :+ Json.obj("hook" -> j))) *> Ok(Json.obj())
    }.orNotFound)

  test("ChatMessage: イベントを見出し・文・リンクにする") {
    val payload = Json.obj(
      "project" -> Json.obj("key" -> "DEMO".asJson, "name" -> "デモ".asJson),
      "task"    -> Json.obj("key" -> "DEMO-12".asJson, "title" -> "ログイン画面".asJson),
      "actor"   -> Json.obj("name" -> "山田".asJson),
      "detail"  -> Json.obj("from" -> "作業前".asJson, "to" -> "進行中".asJson))
    val m = ChatMessage.format(EventKind.StatusChanged, payload, "https://skea.example.com/")
    assertEquals(m.title, "🔄 DEMO-12 ログイン画面")
    assertEquals(m.text, "山田 がステータスを「作業前」から「進行中」に変えました")
    assertEquals(m.url, "https://skea.example.com/tasks/DEMO-12")
    val u = ChatMessage.format(EventKind.TaskUpdated, payload.deepMerge(Json.obj("detail" -> Json.obj("changes" -> Json.arr(
      Json.obj("field" -> "dueDate".asJson), Json.obj("field" -> "assigneeId".asJson))))), "https://skea.example.com")
    assertEquals(u.text, "山田 がタスクを更新しました（期日・担当者）")
    val sp = ChatMessage.format(EventKind.SprintStarted, payload.deepMerge(Json.obj("task" -> Json.Null, "detail" -> Json.obj("name" -> "スプリント 3".asJson))),
      "https://skea.example.com")
    assertEquals((sp.title, sp.url), ("🏃 [DEMO] スプリント 3", "https://skea.example.com/p/DEMO/board"))
  }

  withEnv("stui のチャンネルへの通知: 連携した人だけが設定でき、その人のトークンでチャンネルに投稿する") { env =>
    for
      status <- Ref.of[IO, Status](Status.Created)
      got    <- Ref.of[IO, List[Json]](Nil)
      fake    = new FakeStui(status, got)
      cfg     = apiCfg.copy(linkServices = List(LinkService("stui", "http://stui.test", "skea", "secret")))
      tokens  = new IntegrationTokenStore(xaFixture())
      stui    = new StuiChannels(tokens, fake.client, box, cfg)
      hooks   = new WebhookService(env.integ, fake.client, cfg, stui)
      routes  = new IntegrationRoutes(env.integ, env.projects, env.users, env.activity, hooks, stui, env.engine, null, box, env.sess, env.access, cfg).routes
      call    = (t: String, m: Method, path: String, b: Option[Json]) =>
                  val base = Request[IO](m, Uri.unsafeFromString(path)).addCookie("skea_session", t).putHeaders("Origin" -> publicUrl)
                  routes.orNotFound.run(b.fold(base)(base.withEntity(_))).flatMap(r =>
                    r.bodyText.compile.string.map(x => (r.status, io.circe.parser.parse(x).getOrElse(Json.Null))))
      ua     <- env.user("管理者")
      (admin, at) = ua
      key     = "S" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid    <- env.projects.create(key, "通知", "", admin.id).map(_.toOption.get)
      req     = WebhookReq("", "", List(EventKind.StatusChanged), true, Some(WebhookKind.Stui), Some("c-dev"), Some("dev")).asJson
      // 連携していなければ、チャンネルも読めず、保存もできない
      _      <- call(at, Method.GET, "/api/integrations/stui/channels", None).map(r => assertEquals(r._1, Status.Conflict))
      _      <- call(at, Method.POST, s"/api/projects/$pid/webhooks", Some(req)).map(r => assertEquals(r._1, Status.BadRequest))
      _      <- tokens.saveToken(admin.id, "stui", box.encrypt("stui_token_admin"))
      chs    <- call(at, Method.GET, "/api/integrations/stui/channels", None)
      _       = assertEquals(chs._2.as[List[StuiChannelDto]].toOption.get.map(_.name), List("dev", "ops"))
      made   <- call(at, Method.POST, s"/api/projects/$pid/webhooks", Some(req))
      _       = assertEquals(made._1, Status.Created)
      w       = made._2.as[WebhookDto].toOption.get
      _       = assertEquals((w.kind, w.name, w.url, w.secret, w.channelName, w.owner.map(_.id)), (WebhookKind.Stui, "stui #dev", "", "", Some("dev"), Some(admin.id)))
      // テスト送信: チャンネルを付けて、ボットの通知として送る
      test   <- call(at, Method.POST, s"/api/webhooks/${w.id}/test", None)
      _       = assertEquals(test._2.hcursor.get[Boolean]("ok").toOption, Some(true))
      sent   <- got.get
      _       = assertEquals(sent.last.hcursor.get[String]("channel").toOption, Some("c-dev"))
      _       = assertEquals(sent.last.hcursor.get[String]("title").toOption, Some("skea からのテスト通知"))
      // 送信のワーカー: 401 なら連携を外し、それ以降は送らない
      row    <- env.integ.webhook(w.id).map(_.get)
      d       = DeliveryRow(UUID.randomUUID(), row.id, row.url, row.secret, EventKind.StatusChanged, Json.obj(), 0, row.kind, row.ownerId, row.channelId)
      _      <- status.set(Status.Unauthorized)
      r1     <- hooks.send(d)
      _       = assert(r1._1.isInstanceOf[OutboxResult.Skip], r1.toString)
      _      <- tokens.token(admin.id, "stui").map(t => assertEquals(t, None))
      n      <- got.get.map(_.size)
      r2     <- hooks.send(d)
      _       = assert(r2._1.toString.contains("連携を外しています"), r2.toString)
      _      <- got.get.map(xs => assertEquals(xs.size, n))
    yield ()
  }

  withEnv("チャンネルの管理者は、stui に受信 Webhook を作って送れる（作った人の連携がなくても届き、消すと受信 Webhook も消す）") { env =>
    for
      status <- Ref.of[IO, Status](Status.Created)
      got    <- Ref.of[IO, List[Json]](Nil)
      fake    = new FakeStui(status, got)
      cfg     = apiCfg.copy(linkServices = List(LinkService("stui", "https://chat.example.com", "skea", "secret", Some("http://stui.internal"))))
      tokens  = new IntegrationTokenStore(xaFixture())
      stui    = new StuiChannels(tokens, fake.client, box, cfg)
      hooks   = new WebhookService(env.integ, fake.client, cfg, stui)
      routes  = new IntegrationRoutes(env.integ, env.projects, env.users, env.activity, hooks, stui, env.engine, null, box, env.sess, env.access, cfg).routes
      call    = (t: String, m: Method, path: String, b: Option[Json]) =>
                  val base = Request[IO](m, Uri.unsafeFromString(path)).addCookie("skea_session", t).putHeaders("Origin" -> publicUrl)
                  routes.orNotFound.run(b.fold(base)(base.withEntity(_))).flatMap(r =>
                    r.bodyText.compile.string.map(x => (r.status, io.circe.parser.parse(x).getOrElse(Json.Null))))
      ua     <- env.user("管理者")
      (admin, at) = ua
      key     = "H" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid    <- env.projects.create(key, "受信", "", admin.id).map(_.toOption.get)
      _      <- tokens.saveToken(admin.id, "stui", box.encrypt("stui_token_admin"))
      chs    <- call(at, Method.GET, "/api/integrations/stui/channels", None)
      _       = assertEquals(chs._2.as[List[StuiChannelDto]].toOption.get.map(c => c.name -> c.admin), List("dev" -> true, "ops" -> false))
      // 管理者でないチャンネルには作れない
      bad    <- call(at, Method.POST, s"/api/projects/$pid/webhooks",
                  Some(WebhookReq("", "", Nil, true, Some(WebhookKind.StuiHook), Some("c-ops"), Some("ops")).asJson))
      _       = assertEquals(bad._1, Status.BadRequest)
      made   <- call(at, Method.POST, s"/api/projects/$pid/webhooks",
                  Some(WebhookReq("", "", Nil, true, Some(WebhookKind.StuiHook), Some("c-dev"), Some("dev")).asJson))
      _       = assertEquals(made._1, Status.Created)
      w       = made._2.as[WebhookDto].toOption.get
      _       = assertEquals((w.kind, w.url, w.secret), (WebhookKind.StuiHook, "", ""))
      row    <- env.integ.webhook(w.id).map(_.get)
      // 送る先は、応答の URL のホストではなく、設定した stui のサーバー（STUI_INTERNAL_URL）
      _       = assertEquals((row.url, row.stuiHookId), ("http://stui.internal/hooks/tok_123", Some("h-1")))
      // 作った人が連携を外しても届く
      _      <- tokens.deleteToken(admin.id, "stui")
      d       = DeliveryRow(UUID.randomUUID(), row.id, row.url, row.secret, EventKind.TaskCreated, Json.obj(), 0, row.kind, row.ownerId, row.channelId)
      r      <- hooks.send(d)
      _       = assertEquals(r._1, OutboxResult.Sent)
      sent   <- got.get
      _       = assert(sent.last.hcursor.downField("hook").get[String]("text").toOption.exists(_.contains("がタスクを作成しました")), sent.last.noSpaces)
      // 名前・イベントだけを変えるなら、同じ受信 Webhook を使い続ける（連携がなくても保存できる）
      _      <- call(at, Method.PUT, s"/api/webhooks/${w.id}",
                  Some(WebhookReq("通知", "", List(EventKind.CommentAdded), true, Some(WebhookKind.StuiHook), Some("c-dev"), Some("dev")).asJson))
                  .map(r => assertEquals(r._1, Status.NoContent))
      _      <- env.integ.webhook(w.id).map(x => assertEquals((x.get.name, x.get.stuiHookId), ("通知", Some("h-1"))))
      // 消すと、stui の受信 Webhook も消す（作った人の連携で）
      _      <- tokens.saveToken(admin.id, "stui", box.encrypt("stui_token_admin"))
      _      <- call(at, Method.DELETE, s"/api/webhooks/${w.id}", None).map(r => assertEquals(r._1, Status.NoContent))
      _      <- got.get.map(xs => assertEquals(xs.last, Json.obj("deleted" -> "h-1".asJson)))
    yield ()
  }
