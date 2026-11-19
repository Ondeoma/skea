package skea.api

import sbirds.links.IntegrationTokenStore

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Authorization
import skea.auth.SecretBox
import skea.core.*
import skea.db.*
import skea.shared.*

import java.util.UUID
import scala.concurrent.duration.*

/**
 * stui のチャンネルへの投稿（プロジェクトの通知. docs/stui-integration.md）。設定した人が stui と OAuth で連携したトークンで、
 * その人が参加しているチャンネルに、stui のボット「skea」として投稿する（POST /api/apps/notify の channel）。
 */
final class StuiChannels(
  tokens: IntegrationTokenStore,
  http: Client[IO],
  box: SecretBox,
  cfg: ApiConfig) {

  private val Provider = "stui"

  private def service: Option[LinkService] = cfg.linkServices.find(_.provider == Provider)

  def configured: Boolean = service.isDefined

  private def token(
    userId: UUID
  ): IO[Option[String]] = tokens.token(userId, Provider).map(_.flatMap(box.decrypt))

  def connected(
    userId: UUID
  ): IO[Boolean] = token(userId).map(_.isDefined)

  private def bearer(
    t: String
  ) = Authorization(Credentials.Token(AuthScheme.Bearer, t))

  /** 投稿先に選べるチャンネル（参加している公開・非公開のもの. アーカイブしたものは除く. admin はチャンネルの管理者）。連携していなければ Left。 */
  def channels(
    userId: UUID
  ): IO[Either[AppError, List[StuiChannelDto]]] =
    service match {
      case None => IO.pure(Left(AppError(ErrorCode.StuiNotConfigured)))
      case Some(c) =>
        token(userId).flatMap {
          case None => IO.pure(Left(AppError(ErrorCode.StuiNotConnected)))
          case Some(t) =>
            val req = Request[IO](Method.GET, Uri.unsafeFromString(s"${c.server}/api/channels")).putHeaders(bearer(t))
            http.run(req).use { r =>
              if r.status == Status.Unauthorized then
                tokens.deleteToken(userId, Provider).as(Left(AppError(ErrorCode.StuiConnectionExpired)))
              else if !r.status.isSuccess then
                IO.pure(Left(AppError(
                  ErrorCode.StuiChannelsUnreadable,
                  s"stui のチャンネルを読めませんでした（HTTP ${r.status.code}）"
                )))
              else
                r.as[Json].map(j =>
                  Right(j.asArray.getOrElse(Vector.empty).toList.flatMap { x =>
                    val h = x.hcursor
                    for {
                      id <- h.get[String]("id").toOption
                      name <- h.get[String]("name").toOption
                      kind <- h.get[String]("kind").toOption if kind == "public" || kind == "private"
                      if !h.get[Boolean]("archived").getOrElse(false) && h.get[Boolean]("member").getOrElse(true)
                    } yield StuiChannelDto(id, name, kind, h.get[String]("role").toOption.contains("admin"))
                  }.sortBy(_.name.toLowerCase))
                )
            }.timeout(10.seconds).handleError(_ => Left(AppError(ErrorCode.StuiUnreachable)))
        }
    }

  private def withToken[A](
    userId: UUID
  )(
    f: (LinkService, String) => IO[Either[String, A]]
  ): IO[Either[String, A]] =
    service.fold(IO.pure(Left("stui との連携が設定されていません")))(c =>
      token(userId).flatMap(_.fold(IO.pure(Left("stui と連携してください")))(t => f(c, t)))
    )

  /**
   * 本人の連携で、チャンネルに受信 Webhook を作る（チャンネルの管理者だけが作れる）。(受信 Webhook の ID, 送る URL)。
   * stui が返す URL（{stui の URL}/hooks/{トークン}）から /hooks/{トークン} だけを取り、設定した stui のサーバーの URL（STUI_INTERNAL_URL）に付ける
   * （応答の URL のホストには送らない）。
   */
  private val hookPath = "/hooks/[A-Za-z0-9_=-]+$".r

  def createHook(
    userId: UUID,
    channelId: String,
    name: String
  ): IO[Either[String, (String, String)]] =
    withToken(userId) {
      (
        c,
        t
      ) =>
        {
          val body = Json.obj(
            "channelId" -> channelId.asJson,
            "name" -> s"skea: $name".take(100).asJson,
            "username" -> "skea".asJson,
            "iconUrl" -> Json.Null,
            "enabled" -> true.asJson
          )
          http.run(Request[IO](
            Method.POST,
            Uri.unsafeFromString(s"${c.server}/api/hooks/incoming")
          ).withEntity(body).putHeaders(bearer(t))).use { r =>
            if r.status == Status.Unauthorized then
              tokens.deleteToken(userId, Provider).as(Left("stui との連携が切れています（連携し直してください）"))
            else if r.status == Status.Forbidden then IO.pure(Left("受信 Webhook を作れるのは、チャンネルの管理者だけです"))
            else if !r.status.isSuccess then IO.pure(Left(s"stui に受信 Webhook を作れませんでした（HTTP ${r.status.code}）"))
            else
              r.as[Json].map { j =>
                (j.hcursor.downField("hook").get[String]("id"), j.hcursor.get[String]("url")).tupled.toOption
                  .flatMap(
                    (
                      id,
                      url
                    ) => hookPath.findFirstIn(url).map(path => id -> (c.server.stripSuffix("/") + path))
                  )
                  .toRight("stui の応答が正しくありません")
              }
          }.timeout(15.seconds).handleError(_ => Left("stui に接続できませんでした"))
        }
    }

  /** 作った受信 Webhook を消す（作った人の連携で. 消せなくても続ける）。 */
  def deleteHook(
    userId: UUID,
    hookId: String
  ): IO[Unit] =
    withToken(userId)(
      (
        c,
        t
      ) =>
        http.run(Request[IO](
          Method.DELETE,
          Uri.unsafeFromString(s"${c.server}/api/hooks/incoming/$hookId")
        ).putHeaders(bearer(t)))
          .use(_ => IO.pure(Right(()))).timeout(10.seconds)
    ).void.handleError(_ => ())

  /** stui の受信 Webhook に送る（トークンは使わない. URL が鍵）。 */
  def postHook(
    url: String,
    m: ChatMessage.Message
  ): IO[(OutboxResult, Option[Int])] = {
    val text = (if m.url.nonEmpty then s"**[${m.title}](${m.url})**" else s"**${m.title}**") + "\n\n" +
      m.text.take(2000)
    Uri.fromString(url) match {
      case Left(_) => IO.pure((OutboxResult.Skip("受信 Webhook の URL が正しくありません"), None))
      case Right(u) =>
        http.run(Request[IO](Method.POST, u).withEntity(Json.obj("text" -> text.asJson, "username" -> "skea".asJson)))
          .use(r => IO.pure(r.status.code)).timeout(15.seconds).attempt.map {
            case Right(code) if code >= 200 && code < 300 => (OutboxResult.Sent, Some(code))
            case Right(404) => (OutboxResult.Skip("stui の受信 Webhook が見つかりません（消されたかもしれません. 設定し直してください）"), Some(404))
            case Right(403) => (OutboxResult.Skip("このチャンネルには投稿できません（アーカイブされたかもしれません）"), Some(403))
            case Right(code) => (OutboxResult.Retry(s"HTTP $code"), Some(code))
            case Left(e) => (OutboxResult.Retry(s"stui に接続できませんでした（${e.getClass.getSimpleName}）"), None)
          }.flatTap(
            (
              r,
              _
            ) =>
              Integrations.called(
                "stui.hook",
                r match {
                  case OutboxResult.Sent => "ok"
                  case _: OutboxResult.Retry => "retry"
                  case _ => "skip"
                }
              )
          )
    }
  }

  /** チャンネルに投稿する（Webhook の送信のワーカーから）。 */
  def post(
    ownerId: Option[UUID],
    channelId: String,
    m: ChatMessage.Message
  ): IO[(OutboxResult, Option[Int])] =
    (service, ownerId) match {
      case (None, _) => IO.pure((OutboxResult.Skip("stui との連携が設定されていません（STUI_URL など）"), None))
      case (_, None) => IO.pure((OutboxResult.Skip("設定した人がいません（設定し直してください）"), None))
      case (_, _) if channelId.isEmpty => IO.pure((OutboxResult.Skip("チャンネルが選ばれていません"), None))
      case (Some(c), Some(owner)) =>
        token(owner).flatMap {
          case None => IO.pure((OutboxResult.Skip("設定した人が stui との連携を外しています（設定し直してください）"), None))
          case Some(t) =>
            val req = Request[IO](Method.POST, Uri.unsafeFromString(s"${c.server}/api/apps/notify"))
              .withEntity(Json.obj(
                "title" -> m.title.asJson,
                "text" -> m.text.take(2000).asJson,
                "url" -> m.url.asJson,
                "channel" -> channelId.asJson
              ))
              .putHeaders(bearer(t))
            http.run(req).use(r => IO.pure(r.status.code)).timeout(15.seconds).attempt.flatMap {
              case Right(code) if code >= 200 && code < 300 => IO.pure((OutboxResult.Sent, Some(code)))
              // トークンが失効している: 連携を外す（設定した人が連携し直すまで送れない）
              case Right(401) => tokens.deleteToken(
                  owner,
                  Provider
                ).as((OutboxResult.Skip("stui との連携が切れています（設定した人が連携し直してください）"), Some(401)))
              case Right(403) => IO.pure((OutboxResult.Skip("このチャンネルには投稿できません（設定した人がチャンネルを抜けたかもしれません）"), Some(403)))
              case Right(404) => IO.pure((OutboxResult.Skip("チャンネルが見つかりません"), Some(404)))
              case Right(code) => IO.pure((OutboxResult.Retry(s"HTTP $code"), Some(code)))
              case Left(e) => IO.pure((OutboxResult.Retry(s"stui に接続できませんでした（${e.getClass.getSimpleName}）"), None))
            }
        }.flatTap(
          (
            r,
            _
          ) =>
            Integrations.called(
              "stui.channel",
              r match {
                case OutboxResult.Sent => "ok"
                case _: OutboxResult.Retry => "retry"
                case _ => "skip"
              }
            )
        )
    }
}
