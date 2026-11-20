package skea.app

import sbirds.http.Env.*
import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import skea.api.*
import sbirds.auth.OidcConfig
import sbirds.mail.SmtpConfig
import sbirds.storage.S3Config

/** 設定は環境変数から読む。 */
object Config {

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL")
      // 開発用のログインは名前だけでだれにでも（管理者にも）なれるので、手元（localhost）で開くときだけ（sbirds.http.Env.devLogin）
      devLogin <- devLogin(pub)
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match {
        case (Some(i), Some(c), Some(s)) =>
          Right(Some(OidcConfig(
            i,
            c,
            s,
            s"${pub.stripSuffix("/")}/auth/callback",
            opt("OIDC_GROUPS_CLAIM").getOrElse("groups")
          )))
        case (None, None, None) if devLogin => Right(None)
        case (None, None, None) =>
          Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
        case _ if devLogin => Left("DEV_LOGIN=true は OIDC を設定していないときだけ使えます（本番では DEV_LOGIN を外してください）")
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      secret <- opt("APP_SECRET") match {
        case Some(s) if s.toLowerCase.contains("change-me") => Left("APP_SECRET が雛形の値のままです。ランダムな値にしてください")
        case Some(s) if s.length >= 16 => Right(s)
        case Some(_) => Left("APP_SECRET は16文字以上にしてください")
        case None if devLogin => Right("skea-development-secret")
        case None => Left("APP_SECRET（16文字以上の秘密の値）を設定してください")
      }
      clients <- sbirds.oauth.OAuthClient.fromEnv()
    } yield {
      val base = pub.stripSuffix("/")
      AppConfig(
        port = int("HTTP_PORT").getOrElse(8080),
        db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
        oidc = oidc,
        api = ApiConfig(
          adminGroups = opt("ADMIN_GROUPS").map(_.split(',').map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty),
          cookieSecure = base.startsWith("https://"),
          publicUrl = base,
          devLogin = devLogin,
          maxUploadBytes = int("MAX_UPLOAD_MB").getOrElse(100).toLong * 1024 * 1024,
          timezone = opt("TIMEZONE").getOrElse("Asia/Tokyo"),
          webhookAllowHttp = enabled("WEBHOOK_ALLOW_HTTP"),
          oauthClients = clients,
          // SKIWI_URL・SKIWI_CLIENT_ID・SKIWI_CLIENT_SECRET・SKIWI_INTERNAL_URL（SKAWAU_*・STUI_* も同じ）
          linkServices = skea.api.LinkServiceRoutes.providers.flatMap { p =>
            val e = p.toUpperCase
            (opt(s"${e}_URL"), opt(s"${e}_CLIENT_ID"), opt(s"${e}_CLIENT_SECRET")).mapN((u, id, sec) =>
              skea.api.LinkService(
                p,
                u.stripSuffix("/"),
                id,
                sec,
                opt(s"${e}_INTERNAL_URL").map(_.stripSuffix("/"))
              )
            )
          }
        ),
        staticDir = opt("STATIC_DIR"),
        s3 = opt("S3_ENDPOINT").map(ep =>
          S3Config(
            ep,
            opt("S3_REGION").getOrElse("us-east-1"),
            opt("S3_BUCKET").getOrElse("skea"),
            opt("S3_ACCESS_KEY").getOrElse(""),
            opt("S3_SECRET_KEY").getOrElse(""),
            !opt("S3_PATH_STYLE").contains("false")
          )
        ),
        smtp = opt("SMTP_HOST").map { h =>
          val tls = opt("SMTP_TLS").getOrElse("starttls").toLowerCase
          SmtpConfig(
            h,
            int("SMTP_PORT").getOrElse(if tls == "ssl" then 465 else if tls == "none" then 25 else 587),
            opt("SMTP_USER"),
            opt("SMTP_PASSWORD"),
            tls,
            opt("MAIL_FROM").getOrElse(s"skea@$h")
          )
        },
        appSecret = secret,
        nodeId = opt("NODE_ID").getOrElse(java.net.InetAddress.getLocalHost.getHostName + "-" +
          java.util.UUID.randomUUID().toString.take(8))
      )
    }).left.map(new IllegalArgumentException(_))
  )
}
