package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.{ACursor, Json}
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`WWW-Authenticate`, Allow, Authorization}
import skea.core.*
import skea.shared.*

/**
 * MCP（Model Context Protocol）のサーバー。POST /mcp に JSON-RPC を送る（Streamable HTTP。応答は JSON、SSE は使わない）。
 * 認証は APIキー（Bearer）だけ。各ツールは REST API をそのキーで内部から呼ぶので、権限・スコープ・監査は REST API と同じになる。
 */
final class McpRoutes(
  api: HttpRoutes[IO],
  sessions: SessionAuth,
  publicUrl: String) extends Http4sDsl[IO] {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "mcp" => postMcp(req)
    case (GET | DELETE) -> Root / "mcp" => getMcp()
    case GET -> Root / "api" / "endpoints" => getEndpoints()
  }

  private def postMcp(
    req: Request[IO]
  ): IO[Response[IO]] =
    req.headers.get[Authorization] match {
      case Some(auth @ Authorization(Credentials.Token(AuthScheme.Bearer, _))) =>
        sessions.fromRequest(Request[IO](Method.GET, Uri.unsafeFromString("/mcp")).putHeaders(auth)).flatMap {
          case None => IO.pure(unauthorized)
          case Some(session) =>
            val call = Caller(auth)
            req.as[Json].attempt.flatMap {
              case Left(_) => BadRequest(rpcFail(Json.Null, -32700, "parse error"))
              case Right(j) =>
                j.asArray match {
                  case Some(batch) => batch.toList.traverse(handle(call, session, _)).map(_.flatten).flatMap(rs =>
                      if rs.isEmpty then Accepted() else Ok(rs.asJson)
                    )
                  case None => handle(call, session, j).flatMap(_.fold(Accepted())(Ok(_)))
                }
            }
        }
      case _ => IO.pure(unauthorized)
    }

  private def getMcp(): IO[Response[IO]] =
    MethodNotAllowed(Allow(Method.POST))

  private def getEndpoints(): IO[Response[IO]] =
    Ok(ApiCatalog.all.map(e =>
      Json.obj("method" -> e.method.asJson, "path" -> e.path.asJson, "description" -> e.description.asJson)
    ))

  private val base = publicUrl.stripSuffix("/")
  private val Versions = List("2025-06-18", "2025-03-26", "2024-11-05")

  // ------------------------------------------------------------------ REST API の内部呼び出し

  private final class Caller(
    auth: Authorization) {
    def apply(
      method: Method,
      uri: Uri,
      body: Option[Json] = None
    ): IO[Either[String, Json]] = {
      val req0 = Request[IO](method, uri).putHeaders(auth, "X-Skea-Mcp" -> "1")
      val req = body.fold(req0)(req0.withEntity(_))
      api.run(req).value.flatMap {
        case None => IO.pure(Left("404 見つかりません"))
        case Some(r) =>
          r.bodyText.compile.string.map { s =>
            val j = if s.isEmpty then Json.Null else parse(s).getOrElse(Json.fromString(s))
            if r.status.isSuccess then Right(j)
            else Left(s"${r.status.code} ${j.hcursor.get[String]("error").getOrElse(r.status.reason)}")
          }
      }
    }
  }

  private def path(
    segments: String*
  ): Uri = Uri(path = Uri.Path(segments.toVector.map(Uri.Path.Segment(_)), absolute = true))

  // ------------------------------------------------------------------ ツール

  private type Result = IO[Either[String, String]]

  private final case class Tool(
    name: String,
    description: String,
    schema: Json,
    readOnly: Boolean,
    destructive: Boolean = false
  )(
    val run: (Caller, ACursor) => Result) {
    def json: Json = Json.obj(
      "name" -> name.asJson,
      "description" -> description.asJson,
      "inputSchema" -> schema,
      "annotations" -> Json.obj(
        "readOnlyHint" -> readOnly.asJson,
        "destructiveHint" -> destructive.asJson,
        "openWorldHint" -> false.asJson
      )
    )
  }

  private def schema(
    required: List[String],
    props: (String, Json)*
  ): Json =
    Json.obj("type" -> "object".asJson, "properties" -> Json.obj(props*), "required" -> required.asJson)
  private def str(
    d: String
  ): Json = Json.obj("type" -> "string".asJson, "description" -> d.asJson)
  private def int(
    d: String
  ): Json = Json.obj("type" -> "integer".asJson, "description" -> d.asJson)
  private def strs(
    d: String
  ): Json =
    Json.obj("type" -> "array".asJson, "items" -> Json.obj("type" -> "string".asJson), "description" -> d.asJson)
  private def obj(
    d: String
  ): Json = Json.obj("type" -> "object".asJson, "description" -> d.asJson)

  private def need(
    a: ACursor,
    k: String
  ): Either[String, String] = a.get[String](k).toOption.map(_.trim).filter(_.nonEmpty).toRight(s"$k を指定してください")
  private def opt(
    a: ACursor,
    k: String
  ): Option[String] = a.get[String](k).toOption.map(_.trim)
  private def s(
    j: Json,
    k: String
  ): String = j.hcursor.get[String](k).getOrElse("")

  extension [A](
    e: Either[String, A]
  ) {
    private def withArgs(
      f: A => Result
    ): Result = e.fold(err => IO.pure(Left(err)), f)
  }
  extension (
    r: IO[Either[String, Json]]
  ) {
    private def asText: Result = r.map(_.map(_.spaces2))
  }

  /** プロジェクトの設定一式（名前から ID を引くため）。 */
  private def meta(
    call: Caller,
    key: String
  ): IO[Either[String, Json]] =
    call(Method.GET, path("api", "projects", "by-key", key.toUpperCase)).flatMap {
      case Left(e) => IO.pure(Left(e))
      case Right(p) => call(Method.GET, path("api", "projects", s(p, "id"), "meta"))
    }

  private def find(
    list: Json,
    k: String,
    name: String
  ): Option[String] =
    list.asArray.toList.flatten.find(x => s(x, k) == name || s(x, "id") == name).map(x => s(x, "id"))

  /** ユーザーの指定（me・名前・メール・ID）を ID にする。 */
  private def userId(
    call: Caller,
    v: String
  ): IO[Either[String, Option[String]]] =
    v.trim match {
      case "" | "none" | "なし" => IO.pure(Right(None))
      case "me" | "自分" => call(Method.GET, path("api", "me")).map(_.map(j => Some(s(j, "id"))))
      case x if uuidOpt(x).isDefined => IO.pure(Right(Some(x)))
      case x => call(Method.GET, path("api", "users").withQueryParam("q", x)).map(_.flatMap(j =>
          j.asArray.toList.flatten.headOption.map(u => Some(s(u, "id"))).toRight(s"ユーザー「$x」が見つかりません")
        ))
    }

  /** create_task / update_task の引数を REST API の本文にする。 */
  private def taskBody(
    call: Caller,
    m: Json,
    a: ACursor
  ): IO[Either[String, Json]] = {
    val c = m.hcursor
    def opt2(
      k: String
    ) = a.downField(k).focus.filterNot(_.isNull).map(_.asString.getOrElse("")).map(_.trim)
    (for {
      status <- opt2("status").traverse(n =>
        IO.pure(find(c.downField("statuses").focus.getOrElse(Json.Null), "name", n).toRight(s"ステータス「$n」がありません"))
      )
      sprint <- opt2("sprint").traverse { n =>
        val ss = c.downField("sprints").focus.getOrElse(Json.Null)
        IO.pure(n match {
          case "backlog" | "none" | "バックログ" => Right(Json.Null)
          case "active" => ss.asArray.toList.flatten.find(x => s(x, "state") == "active").map(x =>
              s(x, "id").asJson
            ).toRight("進行中のスプリントがありません")
          case x => find(ss, "name", x).map(_.asJson).toRight(s"スプリント「$x」がありません")
        })
      }
      assignee <- opt2("assignee").traverse(v => userId(call, v))
      labels <- IO.pure(a.get[List[String]]("labels").toOption.traverse(ls =>
        ls.traverse(n =>
          find(
            c.downField("labels").focus.getOrElse(Json.Null),
            "name",
            n
          ).toRight(s"ラベル「$n」がありません（先に call_api でラベルを作ってください）")
        )
      ))
      parent <- opt2("parentKey").filter(_.nonEmpty).traverse(k =>
        call(Method.GET, path("api", "tasks", k)).map(_.map(j => s(j.hcursor.downField("task").focus.get, "id")))
      )
      fields <- IO.pure(a.downField("fields").as[Map[String, Json]].toOption.traverse(fs =>
        fs.toList.traverse(
          (
            n,
            v
          ) =>
            find(c.downField("fields").focus.getOrElse(Json.Null), "name", n).toRight(s"フィールド「$n」がありません").map(_ -> v)
        ).map(Json.fromFields)
      ))
    } yield (status.sequence, sprint.sequence, assignee.sequence, labels, parent.sequence, fields).mapN {
      (
        st,
        sp,
        as,
        lb,
        pa,
        fs
      ) =>
        Json.fromFields(List(
          opt(a, "title").map("title" -> _.asJson),
          // 説明はエディタの文書（JSON）で持つので、Markdown を文書にする（文字列のままだと画面に出ない）
          opt(a, "description").map(d => "description" -> RichText.fromMarkdown(d)),
          st.map("statusId" -> _.asJson),
          sp.map("sprintId" -> _),
          as.map(x => "assigneeId" -> x.asJson),
          lb.map("labelIds" -> _.asJson),
          pa.map("parentId" -> _.asJson),
          opt2("dueDate").map(d => "dueDate" -> (if d.isEmpty then Json.Null else d.asJson)),
          opt2("startDate").map(d => "startDate" -> (if d.isEmpty then Json.Null else d.asJson)),
          opt2("priority").map(p => "priority" -> p.asJson),
          opt2("type").map(t => "taskType" -> t.asJson),
          fs.map("fields" -> _)
        ).flatten)
    })
  }

  private val taskProps = List(
    "title" -> str("タイトル"),
    "description" -> str("説明（Markdown）"),
    "status" -> str("ステータス名"),
    "assignee" -> str("担当者（me・名前・メール・ID。空文字で外す）"),
    "labels" -> strs("ラベル名の配列（置き換え）"),
    "sprint" -> str("スプリント名・active・backlog"),
    "dueDate" -> str("期日 yyyy-MM-dd（空文字で外す）"),
    "startDate" -> str("開始日 yyyy-MM-dd"),
    "priority" -> str("優先度 highest / high / medium / low / lowest"),
    "type" -> str("種類 task / bug / story / epic / improvement"),
    "parentKey" -> str("親タスクのID（SKEA-1）"),
    "fields" -> obj("カスタムフィールド {フィールド名: 値}")
  )

  private val tools: List[Tool] = List(
    Tool("get_me", "自分（この APIキーの持ち主）の情報とキーのスコープ。", schema(Nil), readOnly = true)(
      (
        call,
        _
      ) => call(Method.GET, path("api", "me")).asText
    ),
    Tool("list_projects", "見られるプロジェクトの一覧（キー・名前・自分の権限）。", schema(Nil), readOnly = true) {
      (
        call,
        _
      ) =>
        call(Method.GET, path("api", "projects")).map(_.map(j =>
          j.asArray.toList.flatten.map(p =>
            s"- ${s(p, "key")}: ${s(p, "name")}（${s(p, "role")}） id=${s(p, "id")}"
          ).mkString("\n")
        ))
    },
    Tool(
      "get_project",
      "プロジェクトの設定（ステータス・ラベル・スプリント・カスタムフィールド・メンバー）。",
      schema(List("projectKey"), "projectKey" -> str("プロジェクトのキー")),
      readOnly = true
    )(
      (
        call,
        a
      ) => need(a, "projectKey").withArgs(k => meta(call, k).asText)
    ),
    Tool(
      "search_tasks",
      "タスクを検索する。query は全文検索の語と絞り込み構文（project:SKEA status:進行中 assignee:me label:バグ sprint:active due<2026-12-01 is:open is:overdue has:attachment field:名前=値 order:-updated）。",
      schema(List("query"), "query" -> str("検索の条件"), "limit" -> int("件数（既定50）")),
      readOnly = true
    ) {
      (
        call,
        a
      ) =>
        need(a, "query").withArgs(q =>
          call(
            Method.GET,
            path("api", "search").withQueryParam("q", q).withQueryParam("limit", a.get[Int]("limit").getOrElse(50))
          ).map(_.flatMap { j =>
            j.hcursor.get[String]("error").toOption match {
              case Some(e) => Left(e)
              case None =>
                val hits = j.hcursor.downField("hits").as[List[Json]].getOrElse(Nil)
                Right((s"${j.hcursor.get[Int]("total").getOrElse(0)} 件" :: hits.map { h =>
                  {
                    val t = h.hcursor.downField("task")
                    s"- ${t.get[String]("key").getOrElse("")} ${t.get[String]("title").getOrElse("")}（期日 ${t.get[String]("dueDate").getOrElse("なし")}、担当 ${t.downField("assignee").get[String]("name").getOrElse("なし")}）${
                        if h.hcursor.get[Boolean]("fuzzy").getOrElse(false) then "［あいまい一致］" else ""
                      } $base/tasks/${t.get[String]("key").getOrElse("")}"
                  }
                }).mkString("\n"))
            }
          })
        )
    },
    Tool(
      "get_task",
      "タスクの詳細（説明・サブタスク・リンク・カスタムフィールド・コメント・作業時間）。",
      schema(List("key"), "key" -> str("タスクID（SKEA-12）")),
      readOnly = true
    ) {
      (
        call,
        a
      ) =>
        need(a, "key").withArgs(k =>
          call(Method.GET, path("api", "tasks", k)).flatMap {
            case Left(e) => IO.pure(Left(e))
            case Right(d) =>
              val id = d.hcursor.downField("task").get[String]("id").getOrElse(k)
              (
                call(Method.GET, path("api", "tasks", id, "comments")),
                call(Method.GET, path("api", "tasks", id, "worklogs"))
              ).tupled.map {
                (
                  cs,
                  ws
                ) =>
                  Right(Json.obj(
                    "task" -> d.hcursor.downField("task").focus.getOrElse(Json.Null),
                    "description" -> RichText.toPlain(
                      d.hcursor.downField("description").focus.filterNot(_.isNull).getOrElse(RichText.empty)
                    ).asJson,
                    "subtasks" -> d.hcursor.downField("subtasks").focus.getOrElse(Json.Null),
                    "links" -> d.hcursor.downField("links").focus.getOrElse(Json.Null),
                    "predecessors" -> d.hcursor.downField("predecessors").focus.getOrElse(Json.Null),
                    "fields" -> d.hcursor.downField("fields").focus.getOrElse(Json.Null),
                    "commits" -> d.hcursor.downField("commits").focus.getOrElse(Json.Null),
                    "comments" -> cs.toOption.flatMap(_.asArray).toList.flatten.map(c =>
                      Json.obj(
                        "id" -> c.hcursor.get[String]("id").getOrElse("").asJson,
                        "author" -> c.hcursor.downField("author").get[String]("name").getOrElse("").asJson,
                        "at" -> c.hcursor.get[String]("createdAt").getOrElse("").asJson,
                        "text" -> RichText.toPlain(c.hcursor.downField("body").focus.getOrElse(RichText.empty)).asJson
                      )
                    ).asJson,
                    "worklogs" -> ws.getOrElse(Json.arr()),
                    "url" -> s"$base/tasks/$k".asJson
                  ).spaces2)
              }
          }
        )
    },
    Tool(
      "create_task",
      "タスクを作る。",
      schema(List("projectKey", "title"), (("projectKey" -> str("プロジェクトのキー")) :: taskProps)*),
      readOnly = false
    ) {
      (
        call,
        a
      ) =>
        need(a, "projectKey").withArgs(k =>
          meta(call, k).flatMap {
            case Left(e) => IO.pure(Left(e))
            case Right(m) => taskBody(call, m, a).flatMap {
                case Left(e) => IO.pure(Left(e))
                case Right(b) => call(
                    Method.POST,
                    path("api", "projects", m.hcursor.downField("project").get[String]("id").getOrElse(""), "tasks"),
                    Some(b)
                  )
                    .map(_.map(t => s"${s(t, "key")} を作りました: $base/tasks/${s(t, "key")}"))
              }
          }
        )
    },
    Tool(
      "update_task",
      "タスクを変える（指定したものだけ）。",
      schema(List("key"), (("key" -> str("タスクID")) :: taskProps)*),
      readOnly = false
    ) {
      (
        call,
        a
      ) =>
        need(a, "key").withArgs(k =>
          call(Method.GET, path("api", "tasks", k)).flatMap {
            case Left(e) => IO.pure(Left(e))
            case Right(d) =>
              val pid = d.hcursor.downField("task").get[String]("projectId").getOrElse("")
              val tid = d.hcursor.downField("task").get[String]("id").getOrElse("")
              call(Method.GET, path("api", "projects", pid, "meta")).flatMap {
                case Left(e) => IO.pure(Left(e))
                case Right(m) => taskBody(call, m, a).flatMap {
                    case Left(e) => IO.pure(Left(e))
                    case Right(b) =>
                      call(Method.PATCH, path("api", "tasks", tid), Some(b)).map(_.map(_ => s"$k を更新しました"))
                  }
              }
          }
        )
    },
    Tool(
      "delete_task",
      "タスクを削除する（サブタスクも）。",
      schema(List("key"), "key" -> str("タスクID")),
      readOnly = false,
      destructive = true
    ) {
      (
        call,
        a
      ) =>
        need(a, "key").withArgs(k =>
          call(Method.GET, path("api", "tasks", k)).flatMap {
            case Left(e) => IO.pure(Left(e))
            case Right(d) => call(
                Method.DELETE,
                path("api", "tasks", d.hcursor.downField("task").get[String]("id").getOrElse(""))
              ).map(_.map(_ => s"$k を削除しました"))
          }
        )
    },
    Tool(
      "add_comment",
      "タスクにコメントする（Markdown。@メンションは使えない）。",
      schema(List("key", "text"), "key" -> str("タスクID"), "text" -> str("本文（Markdown）")),
      readOnly = false
    ) {
      (
        call,
        a
      ) =>
        (need(a, "key"), need(a, "text")).tupled.withArgs(
          (
            k,
            t
          ) =>
            call(Method.POST, path("api", "tasks", k, "comments"), Some(Json.obj("body" -> t.asJson))).map(_.map(c =>
              s"コメントしました: $base/tasks/$k#comment-${s(c, "id")}"
            ))
        )
    },
    Tool(
      "log_work",
      "作業時間を記録する。",
      schema(
        List("key", "time"),
        "key" -> str("タスクID"),
        "time" -> str("時間（1h30m、90m、1.5h）"),
        "date" -> str("作業日 yyyy-MM-dd（既定は今日）"),
        "note" -> str("メモ")
      ),
      readOnly = false
    ) {
      (
        call,
        a
      ) =>
        (need(a, "key"), need(a, "time").flatMap(t => Minutes.parse(t).toRight(s"時間として読めません: $t"))).tupled.withArgs(
          (
            k,
            m
          ) =>
            call(
              Method.POST,
              path("api", "tasks", k, "worklogs"),
              Some(Json.obj(
                "workDate" -> opt(a, "date").filter(
                  _.nonEmpty
                ).getOrElse(java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo")).toString).asJson,
                "minutes" -> m.asJson,
                "note" -> opt(a, "note").asJson
              ))
            ).map(_.map(_ => s"$k に ${Minutes.format(m)} を記録しました"))
        )
    },
    Tool(
      "list_worklogs",
      "作業時間の一覧（誰がいつ何に何時間）。",
      schema(
        Nil,
        "from" -> str("開始日 yyyy-MM-dd（既定は今週の月曜）"),
        "to" -> str("終了日"),
        "projectKey" -> str("プロジェクトで絞る"),
        "user" -> str("ユーザーで絞る（me・名前）")
      ),
      readOnly = true
    ) {
      (
        call,
        a
      ) =>
        val pidF = opt(a, "projectKey").filter(_.nonEmpty).traverse(k =>
          call(Method.GET, path("api", "projects", "by-key", k.toUpperCase)).map(_.map(p => s(p, "id")))
        )
        (
          pidF.map(_.sequence),
          opt(a, "user").filter(_.nonEmpty).traverse(userId(call, _)).map(_.sequence.map(_.flatten))
        ).tupled.flatMap {
          case (Left(e), _) => IO.pure(Left(e))
          case (_, Left(e)) => IO.pure(Left(e))
          case (Right(pid), Right(uid)) =>
            val q = List("from" -> opt(a, "from"), "to" -> opt(a, "to"), "projectId" -> pid, "userId" -> uid).collect {
              case (k, Some(v)) if v.nonEmpty => k -> v
            }
            call(
              Method.GET,
              q.foldLeft(path("api", "worklogs"))(
                (
                  u,
                  kv
                ) => u.withQueryParam(kv._1, kv._2)
              )
            ).map(_.map(j =>
              j.asArray.toList.flatten.map(w =>
                s"${s(w, "workDate")} ${w.hcursor.downField("user").get[String]("name").getOrElse("")} ${s(w, "taskKey")} ${s(w, "taskTitle")} ${Minutes.format(w.hcursor.get[Int]("minutes").getOrElse(0))} ${s(w, "note")}"
              ).mkString("\n")
            ))
        }
    },
    Tool(
      "get_board",
      "カンバン（アクティブなスプリントのタスクをステータスごとに）。",
      schema(List("projectKey"), "projectKey" -> str("プロジェクトのキー")),
      readOnly = true
    ) {
      (
        call,
        a
      ) =>
        need(a, "projectKey").withArgs(k =>
          meta(call, k).flatMap {
            case Left(e) => IO.pure(Left(e))
            case Right(m) =>
              val pid = m.hcursor.downField("project").get[String]("id").getOrElse("")
              call(Method.GET, path("api", "projects", pid, "board")).map(_.map { b =>
                val ts = b.hcursor.downField("tasks").as[List[Json]].getOrElse(Nil)
                val sprint = b.hcursor.downField("sprint").get[String]("name").getOrElse("（進行中のスプリントなし）")
                (s"スプリント: $sprint" :: m.hcursor.downField("statuses").as[List[Json]].getOrElse(Nil).map { st =>
                  val items = ts.filter(t => s(t, "statusId") == s(st, "id")).map(t =>
                    s"  - ${s(t, "key")} ${s(t, "title")}（${t.hcursor.downField("assignee").get[String]("name").getOrElse("未割り当て")}）"
                  )
                  (s"## ${s(st, "name")}（${items.size}）" :: items).mkString("\n")
                }).mkString("\n")
              })
          }
        )
    },
    Tool(
      "list_notifications",
      "自分への通知（未読・メンションで絞れる）。",
      schema(
        Nil,
        "kinds" -> str("種類（mention,assigned,comment,updated,status,due,automation のカンマ区切り）"),
        "unreadOnly" -> Json.obj("type" -> "boolean".asJson)
      ),
      readOnly = true
    ) {
      (
        call,
        a
      ) =>
        val u =
          List("kinds" -> opt(a, "kinds"), "unread" -> a.get[Boolean]("unreadOnly").toOption.map(_.toString)).collect {
            case (k, Some(v)) => k -> v
          }
            .foldLeft(path("api", "notifications"))(
              (
                u,
                kv
              ) => u.withQueryParam(kv._1, kv._2)
            )
        call(Method.GET, u).asText
    },
    Tool(
      "list_api_endpoints",
      "REST API の一覧。専用のツールがない操作（スプリントの開始・設定の変更など）は call_api で行える。",
      schema(Nil),
      readOnly = true
    ) {
      (
        _,
        _
      ) =>
        IO.pure(Right(ApiCatalog.all.map(e => s"${e.method} ${e.path} — ${e.description}").mkString("\n")))
    },
    Tool(
      "call_api",
      "任意の REST API を呼ぶ（画面でできることはすべてできる。list_api_endpoints を参照）。権限はこのキーの持ち主と同じ。",
      schema(
        List("method", "path"),
        "method" -> str("GET / POST / PUT / PATCH / DELETE"),
        "path" -> str("/api/ で始まるパス（クエリを含めてよい）"),
        "body" -> obj("JSON の本文")
      ),
      readOnly = false
    ) {
      (
        call,
        a
      ) =>
        (need(a, "method").map(_.toUpperCase), need(a, "path")).tupled.withArgs {
          (
            m,
            p
          ) =>
            (
              Method.fromString(m).toOption.filter(x => Set("GET", "POST", "PUT", "PATCH", "DELETE")(x.name)),
              Uri.fromString(p).toOption.filter(_.path.renderString.startsWith("/api/"))
            ) match {
              case (Some(method), Some(uri)) => call(method, uri, a.downField("body").focus.filterNot(_.isNull)).asText
              case (None, _) => IO.pure(Left("method は GET / POST / PUT / PATCH / DELETE です"))
              case _ => IO.pure(Left("path は /api/ で始めてください"))
            }
        }
    }
  )

  private val byName = tools.map(t => t.name -> t).toMap

  // ------------------------------------------------------------------ JSON-RPC

  private def ok(
    id: Json,
    result: Json
  ): Json = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> result)
  private def rpcFail(
    id: Json,
    code: Int,
    msg: String
  ): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "error" -> Json.obj("code" -> code.asJson, "message" -> msg.asJson))
  private def toolResult(
    r: Either[String, String]
  ): Json =
    Json.obj(
      "content" -> Json.arr(Json.obj("type" -> "text".asJson, "text" -> r.merge.asJson)),
      "isError" -> r.isLeft.asJson
    )

  private def handle(
    call: Caller,
    s: AuthSession,
    msg: Json
  ): IO[Option[Json]] = {
    val c = msg.hcursor
    val params = c.downField("params")
    c.downField("id").focus match {
      case None => IO.pure(None)
      case Some(id) =>
        c.get[String]("method").getOrElse("") match {
          case "initialize" =>
            val asked = params.get[String]("protocolVersion").toOption
            IO.pure(Some(ok(
              id,
              Json.obj(
                "protocolVersion" -> asked.filter(Versions.contains).getOrElse(Versions.head).asJson,
                "capabilities" -> Json.obj("tools" -> Json.obj("listChanged" -> false.asJson)),
                "serverInfo" -> Json.obj("name" -> "skea".asJson, "version" -> "0.1.0".asJson),
                "instructions" ->
                  ("skea（チームのタスク管理）のプロジェクト・タスク・スプリント・コメント・作業時間を読み書きするツールです。" +
                    "タスクは search_tasks（絞り込み構文が使える）で探し、get_task で詳細を読みます。タスクIDは SKEA-12 の形です。" +
                    "専用のツールがない操作は list_api_endpoints で REST API を調べ、call_api で行えます。権限はこの APIキーの持ち主と同じです。").asJson
              )
            )))
          case "ping" => IO.pure(Some(ok(id, Json.obj())))
          case "tools/list" => IO.pure(Some(ok(id, Json.obj("tools" -> tools.map(_.json).asJson))))
          case "tools/call" =>
            val name = params.get[String]("name").getOrElse("")
            byName.get(name) match {
              case None => IO.pure(Some(rpcFail(id, -32602, s"unknown tool: $name")))
              case Some(t) if !s.canWrite && !t.readOnly =>
                IO.pure(Some(ok(id, toolResult(Left("参照専用の APIキーでは変更できません")))))
              case Some(t) =>
                val args = params.downField("arguments").focus.filter(_.isObject).getOrElse(Json.obj()).hcursor
                t.run(call, args).handleError(e => Left(s"内部エラー: ${e.getMessage}")).map(r =>
                  Some(ok(id, toolResult(r)))
                )
            }
          case "resources/list" => IO.pure(Some(ok(id, Json.obj("resources" -> Json.arr()))))
          case "prompts/list" => IO.pure(Some(ok(id, Json.obj("prompts" -> Json.arr()))))
          case other => IO.pure(Some(rpcFail(id, -32601, s"method not found: $other")))
        }
    }
  }

  private val unauthorized =
    Response[IO](Status.Unauthorized).putHeaders(`WWW-Authenticate`(Challenge(
      "Bearer",
      "skea"
    ))).withEntity(ErrorDto("APIキー（Authorization: Bearer）が必要です"))

}
