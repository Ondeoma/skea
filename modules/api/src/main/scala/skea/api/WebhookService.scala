package skea.api

import sbirds.shared.AppError

import cats.effect.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import sbirds.auth.Crypto
import skea.core.*
import skea.db.*
import skea.shared.*

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

/**
 * Webhook の送信（アウトボックスのワーカー）。本文に HMAC-SHA256 の署名を付ける。
 * 送信先が stui のチャンネル（kind = stui）なら、本文を文にして stui に投稿する（StuiChannels）。
 */
final class WebhookService(integ: IntegrationStore, http: Client[IO], cfg: ApiConfig, stui: StuiChannels) {

  def validUrl(u: String): Either[AppError, String] = {
    val t = u.trim
    if t.startsWith("https://") || (cfg.webhookAllowHttp && t.startsWith("http://")) then
      Uri.fromString(t).left.map(_ => AppError(ErrorCode.WebhookUrlInvalid)).map(_ => t)
    else
      Left(AppError(
        ErrorCode.WebhookUrlScheme,
        if cfg.webhookAllowHttp then "URL は http(s):// で始めてください" else "URL は https:// で始めてください"
      ))
  }

  /**
   * 送る要求。本文は payload.noSpaces そのもので、署名もこの本文で計算する。
   * CirceEntityCodec を import しているので、文字のエンコーダーを明示しないと本文が JSON の文字列（"{\"event\":...}"）になる。
   */
  def request(d: DeliveryRow): Request[IO] = {
    val body = d.payload.noSpaces
    val sig = "sha256=" + Crypto.hmacHex(d.secret, body.getBytes(StandardCharsets.UTF_8))
    Request[IO](Method.POST, Uri.unsafeFromString(d.url))
      .withEntity(body)(using EntityEncoder.stringEncoder)
      .withContentType(`Content-Type`(MediaType.application.json))
      .putHeaders(
        "X-Skea-Event" -> d.event,
        "X-Skea-Delivery" -> d.id.toString,
        "X-Skea-Signature" -> sig,
        "User-Agent" -> "skea-webhook"
      )
  }

  def send(d: DeliveryRow): IO[(OutboxResult, Option[Int])] =
    if WebhookKind.isStui(d.kind) then {
      val m = ChatMessage.format(d.event, d.payload, cfg.publicUrl)
      sbirds.observe.Telemetry.span("webhook.deliver", "webhook.event" -> d.event, "webhook.kind" -> d.kind)(
        if d.kind == WebhookKind.StuiHook then stui.postHook(d.url, m)
        else stui.post(d.ownerId, d.channelId.getOrElse(""), m)
      )
    } else sendUrl(d)

  private def sendUrl(d: DeliveryRow): IO[(OutboxResult, Option[Int])] = {
    val req = request(d)
    // 宛先は送るたびに確かめる（名前解決の結果が変わっても内部へ送らない）
    NetGuard.check(req.uri).flatMap {
      case Left(m) => IO.pure((OutboxResult.Skip(m), None))
      case Right(_) => sbirds.observe.Telemetry.span("webhook.deliver", "webhook.event" -> d.event)(
          http.run(req).use(r => IO.pure(r.status.code)).timeout(15.seconds).attempt.map {
            case Right(c) if c >= 200 && c < 300 => (OutboxResult.Sent, Some(c))
            case Right(c) if c == 410 => (OutboxResult.Skip(s"HTTP $c"), Some(c))
            case Right(c) => (OutboxResult.Retry(s"HTTP $c"), Some(c))
            // 例外の文（内部のホスト名など）は返さない
            case Left(e) => (OutboxResult.Retry(s"接続できませんでした（${e.getClass.getSimpleName}）"), None)
          }
        )
    }.flatTap {
      (r, _) =>
        Integrations.called(
          "webhook",
          r match {
            case OutboxResult.Sent => "ok"
            case _: OutboxResult.Retry => "retry"
            case _ => "skip"
          }
        )
    }
  }

  def loop: IO[Nothing] =
    Periodic.run(10.seconds, 10.seconds, ErrorCode.WebhookWorkerFailed)(integ.processDeliveries(50)(send))

  def purgeLoop: IO[Nothing] =
    Periodic.run(1.hour, 24.hours, ErrorCode.WebhookPurgeFailed)(integ.purgeDeliveries(30))
}
