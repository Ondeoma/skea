package skea.api

import munit.CatsEffectSuite

/** Webhook の送信の要求（本文と署名）。要求を組み立てるだけなので、ほかの依存は渡さない。 */
class WebhookSpec extends CatsEffectSuite:

  private val svc = new WebhookService(null, null, null, null)

  test("送る本文は payload そのもので、署名もその本文で合う（JSON の文字列にしない）") {
    val payload = io.circe.parser.parse("""{"event":"message.created","text":"改行\nと\"引用符\""}""").toOption.get
    val d       = skea.db.DeliveryRow(java.util.UUID.randomUUID(), java.util.UUID.randomUUID(), "https://example.com/hook", "s3cret", "ticket.created", payload, 0)
    val req     = svc.request(d)
    req.bodyText.compile.string.map { body =>
      assertEquals(body, payload.noSpaces)
      assertEquals(req.headers.get(org.typelevel.ci.CIString("X-Skea-Signature")).map(_.head.value),
        Some("sha256=" + skea.auth.Crypto.hmacHex("s3cret", body.getBytes(java.nio.charset.StandardCharsets.UTF_8))))
      assertEquals(req.contentType.map(_.mediaType), Some(org.http4s.MediaType.application.json))
    }
  }
