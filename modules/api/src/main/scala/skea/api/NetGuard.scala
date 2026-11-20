package skea.api

import cats.effect.*
import org.http4s.Uri
import org.http4s.client.Client
import sbirds.http.Env
import sbirds.net.NetGuard.{Allow, Policy, Rejected}

import scala.concurrent.duration.*

/**
 * 利用者が設定した URL へサーバーから送る前の確認（SSRF 対策. 判定は sbirds.net.NetGuard）。
 * 名前解決したアドレスが loopback・link-local（クラウドのメタデータを含む）・プライベート・ULA などなら拒否する。
 * 社内のサーバーへ送りたいときは OUTBOUND_ALLOW_HOSTS（カンマ区切りのホスト名）か、OUTBOUND_ALLOW_PRIVATE=true で許可する
 * （OUTBOUND_ALLOW_PRIVATE でも loopback・link-local は許可しない）。
 */
object NetGuard {

  private val policy = Policy(
    if Env.enabled("OUTBOUND_ALLOW_PRIVATE") then Allow.Private else Allow.PublicOnly,
    Env.list("OUTBOUND_ALLOW_HOSTS").map(_.toLowerCase).toSet
  )

  /** 断った理由の文言。 */
  def message(r: Rejected): String = r match {
    case Rejected.Internal(host) => s"内部のアドレス（$host）には送れません（OUTBOUND_ALLOW_HOSTS で許可できます）"
    case other => other.message
  }

  /** 送ってよい URL か（http・https で、宛先が内部のアドレスでない）。 */
  def check(uri: Uri): IO[Either[String, Unit]] = sbirds.net.NetGuard.check(uri, policy).map(_.left.map(message))

  def check(url: String): IO[Either[String, Unit]] = sbirds.net.NetGuard.check(url, policy).map(_.left.map(message))

  /**
   * 利用者が設定した URL へ送る HTTP クライアント（Webhook・自動化の Web リクエスト・リポジトリの API）。
   * つなぐたびに名前を引いて確かめ、確かめた IP にそのままつなぐ（check の後で名前の向き先を変えられても内部へはつながない）。
   * リダイレクトは追わない。断ったときは Rejected で失敗する。Main で1つ作って渡す。
   */
  def client: Resource[IO, Client[IO]] = sbirds.net.NetGuard.client(policy, timeout = 30.seconds)
}
