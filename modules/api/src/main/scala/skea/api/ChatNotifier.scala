package skea.api

import sbirds.links.IntegrationTokenStore

import sbirds.observe.Log

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

import java.util.UUID
import scala.concurrent.duration.*

/**
 * 通知をチャット（stui）にも送る（docs/stui-integration.md）。連携したユーザーに、stui のボット「skea」からの DM として届く。
 * 通知の種類ごとに個人設定で止められる。トークンが失効していたら（401）連携を外す。
 */
final class ChatNotifier(
  tokens: IntegrationTokenStore,
  users: UserStore,
  http: Client[IO],
  box: SecretBox,
  cfg: ApiConfig) {

  private val Provider = "stui"

  def send(
    userId: UUID,
    kind: String,
    title: String,
    text: String,
    link: String
  ): IO[Unit] =
    cfg.linkServices.find(_.provider == Provider).traverse_ { c =>
      tokens.token(userId, Provider).map(_.flatMap(box.decrypt)).flatMap(_.traverse_ { token =>
        users.chatWanted(userId, kind).flatMap(IO.whenA(_) {
          val req = Request[IO](Method.POST, Uri.unsafeFromString(s"${c.server}/api/apps/notify"))
            .withEntity(Json.obj(
              "title" -> title.asJson,
              "text" -> text.take(2000).asJson,
              "url" -> s"${cfg.publicUrl}$link".asJson
            ))
            .putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)))
          sbirds.observe.Telemetry.span("stui.notify", "kind" -> kind)(http.run(req).use { r =>
            Integrations.called(Provider, if r.status.isSuccess then "ok" else "error") *>
              (if r.status == Status.Unauthorized then tokens.deleteToken(userId, Provider)
               else IO.whenA(!r.status.isSuccess)(Log.report(ErrorCode.StuiNotifyRejected, s"HTTP ${r.status.code}")))
          }.timeout(10.seconds))
        })
      })
    }.handleErrorWith(e => Log.report(ErrorCode.StuiNotifyFailed, "", e))
}
