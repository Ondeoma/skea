package skea.api

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import sbirds.http.{Csrf, Errors, RouteHelpers}
import sbirds.shared.AppError

import java.util.UUID

trait RouteSupport extends RouteHelpers {
  protected def sessions: SessionAuth
  protected def access: Access2

  protected def bodyInvalid: sbirds.shared.ErrorCode = ErrorCode.RequestBodyInvalid

  // 本文の読み取りの途中のエラー（大きすぎる本文を含む）は、これまでどおり RequestBodyInvalid（400）にする
  override protected def bodyKeepsAppError: Boolean = false

  /** エラーの応答（コード付きでログに出る）. detail を省くとコードの既定の文言. */
  protected def err(code: ErrorCode, detail: String | Null = null): IO[Response[IO]] =
    Errors.respond(AppError(code, Option(detail)))

  protected def fail(e: AppError): IO[Response[IO]] = Errors.respond(e)

  /** 結果が Right なら f、Left なら失敗の応答。 */
  protected def respond[A](r: Res[A])(f: A => IO[Response[IO]]): IO[Response[IO]] = r.flatMap(_.fold(fail, f))

  protected def okJson[A: io.circe.Encoder](r: Res[A]): IO[Response[IO]] = respond(r)(a => Ok(a))

  /** POST だが読み取りだけの API（本文で条件を送るもの）。 */
  private val readOnlyPosts = Set("/api/tasks/lookup")
  private def readOnlyPost(req: Request[IO]): Boolean = req.method == Method.POST &&
    readOnlyPosts.contains(req.pathInfo.renderString)

  private val safeMethods = Set(Method.GET, Method.HEAD, Method.OPTIONS)

  /**
   * Cookie のセッションでの更新系の要求が、別のサイトから送られたものでないか（CSRF 対策）。
   * ブラウザが付ける Origin（なければ Referer）と Sec-Fetch-Site を PUBLIC_URL と照らす。開発中（DEV_LOGIN）は http の localhost も許す。
   */
  private lazy val csrf =
    Csrf.Policy(sessions.publicUrl, devLocalhost = sessions.devLogin).copy(safeMethods = Csrf.SafeWithOptions)

  /** ログインしていなければ 401。更新系の要求を read スコープのキーで送ったら 403。 */
  protected def authed(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None => err(ErrorCode.NotLoggedIn)
      case Some(s) =>
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        sbirds.observe.Telemetry.withUser(s.user.id.toString, s.user.name) {
          if s.isSession && Csrf.foreign(req, csrf) then err(ErrorCode.CrossSiteRequest)
          else if !s.canWrite && !safeMethods.contains(req.method) && !readOnlyPost(req) then
            err(ErrorCode.ApiKeyNeedsWrite)
          else f(s)
        }
    }

  /** システム管理者のみ（APIキーは admin スコープが必要）。 */
  protected def admin(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => if s.canAdmin then f(s) else err(ErrorCode.AdminOnly))

  /** Cookie のセッションのみ（APIキーの発行など）。 */
  protected def sessionOnly(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => if s.isSession then f(s) else err(ErrorCode.SessionOnly))

  /** プロジェクトの権限を確認してから処理する。 */
  protected def withProject(req: Request[IO],
                            pid: UUID,
                            need: Role)(f: (AuthSession, Role) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => respond(access.project(s, pid, need))(r => f(s, r)))

  /** タスクの権限を確認してから処理する。 */
  protected def withTask(req: Request[IO],
                         id: String,
                         need: Role)(f: (AuthSession, TaskCore, Role) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s =>
      respond(access.task(s, id, need))((t, r) => f(s, t, r))
    )

  /** multipart/form-data の file を一時ファイルに受け取る（上限を超えたら 413）。 */
  protected def withUpload(req: Request[IO], maxBytes: Long)(f: Upload => IO[Response[IO]]): IO[Response[IO]] =
    req.as[org.http4s.multipart.Multipart[IO]].attempt.flatMap {
      case Left(_) => err(ErrorCode.UploadNotMultipart)
      case Right(mp) =>
        mp.parts.find(_.name.contains("file")) match {
          case None => err(ErrorCode.UploadFileMissing)
          case Some(part) =>
            val name =
              part.filename.map(_.trim).filter(_.nonEmpty).getOrElse("file").replaceAll("[\\\\/\\r\\n]", "_").take(200)
            val ct = part.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}").getOrElse(
              "application/octet-stream"
            )
            fs2.io.file.Files[IO].tempFile.use { tmp =>
              part.body.take(maxBytes + 1).through(fs2.io.file.Files[IO].writeAll(tmp)).compile.drain *>
                fs2.io.file.Files[IO].size(tmp).flatMap { size =>
                  if size > maxBytes then
                    err(ErrorCode.UploadTooLarge, s"ファイルが大きすぎます（上限 ${math.max(1, maxBytes / 1024 / 1024)}MB）")
                  else f(Upload(name, ct, tmp, size))
                }
            }
        }
    }

}
