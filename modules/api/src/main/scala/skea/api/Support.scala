package skea.api

import cats.effect.*
import skea.core.*
import sbirds.shared.AppError

import java.time.{LocalDate, ZoneId}
import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(adminGroups: Set[String],
                           cookieSecure: Boolean,
                           publicUrl: String,
                           sessionTtl: FiniteDuration = 14.days,
                           cookieName: String = "skea_session",
                           devLogin: Boolean = false,
                           maxUploadBytes: Long = 100L * 1024 * 1024,
                           timezone: String = "Asia/Tokyo",
                           webhookAllowHttp: Boolean = false,
                           version: String = "0.1.0",
                           oauthClients: List[OAuthClient] = Nil,
                           linkServices: List[LinkService] = Nil)

/**
 * OAuth で連携する外部サービス（skiwi・skawau: リンクに名前を出す、stui: 通知を送る）。相手に skea をクライアントとして登録しておく。
 * url は利用者のブラウザから見た URL（許可画面・貼られるリンク）、internalUrl はサーバーから呼ぶときの URL（省略時は url）。
 */
final case class LinkService(provider: String,
                             url: String,
                             clientId: String,
                             clientSecret: String,
                             internalUrl: Option[String] = None) {
  def server: String = internalUrl.getOrElse(url)
}

/** 認可を求める外部のサービス（OAUTH_CLIENTS で登録する）。redirectUri は完全一致で照合する。 */
/** scopes はそのクライアントが求めてよいスコープ（read・write. 既定は read だけ）. */
final case class OAuthClient(id: String,
                             secret: String,
                             redirectUri: String,
                             name: String,
                             scopes: List[String] = List("read"))

/** 処理の結果（失敗は AppError: エラーコードと文言）. */
type Res[A] = IO[Either[AppError, A]]

/** 操作する人と、自動化の連鎖の深さ（ルールが別のルールを起動する深さ）。 */
final case class Op(s: AuthSession, depth: Int = 0) {
  def userId: UUID = s.user.id
  def via: String = s.viaString
  def today: LocalDate = LocalDate.now(Op.zone(s.user.timezone))
}

object Op {
  def zone(tz: String): ZoneId = scala.util.Try(ZoneId.of(tz)).getOrElse(ZoneId.of("Asia/Tokyo"))
}

/**
 * *Routes に共通の補助（エラー応答・認証・リクエスト本文・権限の確認）。
 */
/** 受け取ったファイル（一時ファイル）。 */
final case class Upload(name: String, contentType: String, path: fs2.io.file.Path, size: Long)

/** 日付の入力（yyyy-MM-dd）。 */
object Dates {
  def parse(s: Option[String], what: String): Either[AppError, Option[LocalDate]] =
    s.map(_.trim).filter(_.nonEmpty) match {
      case None => Right(None)
      case Some(v) =>
        scala.util.Try(LocalDate.parse(v)).toOption.toRight(AppError(
          ErrorCode.DateFormatInvalid,
          s"$what は yyyy-MM-dd で指定してください"
        )).map(Some(_))
    }
}
