package skea.api

import cats.effect.*
import org.http4s.Uri

import java.net.{Inet6Address, InetAddress}

/**
 * 利用者が設定した URL へサーバーから送る前の確認（SSRF 対策）。
 * 名前解決したアドレスが loopback・link-local（クラウドのメタデータを含む）・プライベート・ULA などなら拒否する。
 * 社内のサーバーへ送りたいときは OUTBOUND_ALLOW_HOSTS（カンマ区切りのホスト名）か、OUTBOUND_ALLOW_PRIVATE=true で許可する
 * （OUTBOUND_ALLOW_PRIVATE でも loopback・link-local は許可しない）。
 */
object NetGuard {

  private val allowHosts: Set[String] =
    sys.env.get(
      "OUTBOUND_ALLOW_HOSTS"
    ).toList.flatMap(_.split(',').toList).map(_.trim.toLowerCase).filter(_.nonEmpty).toSet
  private val allowPrivate: Boolean = sys.env.get("OUTBOUND_ALLOW_PRIVATE").exists(_.trim.equalsIgnoreCase("true"))

  /** 内部向けのアドレスか（private は RFC1918・ULA・CGNAT）。 */
  private def kind(
    a: InetAddress
  ): Option[String] = {
    val b = a.getAddress.map(_ & 0xff)
    if a.isLoopbackAddress || a.isAnyLocalAddress || a.isLinkLocalAddress || a.isMulticastAddress then Some("local")
    else if a.isSiteLocalAddress then Some("private")
    else
      a match {
        case _: Inet6Address if (b(0) & 0xfe) == 0xfc => Some("private") // fc00::/7
        case _: Inet6Address if b.take(10).forall(_ == 0) && b(10) == 0xff && b(11) == 0xff => // ::ffff:a.b.c.d
          kind(InetAddress.getByAddress(a.getAddress.drop(12)))
        case _ if b.length == 4 && b(0) == 100 && (b(1) & 0xc0) == 64 => Some("private") // 100.64.0.0/10
        case _ if b.length == 4 && b(0) == 0 => Some("local")
        case _ => None
      }
  }

  /** 送ってよい URL か（http・https で、宛先が内部のアドレスでない）。 */
  def check(
    uri: Uri
  ): IO[Either[String, Unit]] =
    (
      uri.scheme.map(_.value.toString.toLowerCase),
      uri.host.map(_.value.stripPrefix("[").stripSuffix("]").toLowerCase)
    ) match {
      case (Some("http" | "https"), Some(host)) if host.nonEmpty =>
        if allowHosts.contains(host) then IO.pure(Right(()))
        else
          IO.blocking(InetAddress.getAllByName(host).toList).attempt.map {
            case Left(_) => Left(s"ホスト $host を名前解決できません")
            case Right(as) =>
              as.flatMap(kind).distinct match {
                case Nil => Right(())
                case ks if allowPrivate && !ks.contains("local") => Right(())
                case _ => Left(s"内部のアドレス（$host）には送れません（OUTBOUND_ALLOW_HOSTS で許可できます）")
              }
          }
      case _ => IO.pure(Left("URL は http:// か https:// で始まるものにしてください"))
    }

  def check(
    url: String
  ): IO[Either[String, Unit]] =
    Uri.fromString(url.trim).fold(_ => IO.pure(Left("URL が正しくありません")), check)
}
