package skea.api

import cats.effect.*

/** 連携（Webhook・stui・SCM）の呼び出しの数（skorimako のメトリクス skea.integrations.calls）。 */
object Integrations {
  def called(integration: String, result: String): IO[Unit] =
    sbirds.observe.Metrics.count("skea.integrations.calls", "integration" -> integration, "result" -> result)
}
