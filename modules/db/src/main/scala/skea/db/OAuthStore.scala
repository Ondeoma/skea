package skea.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

final case class OAuthCode(
  clientId: String,
  redirectUri: String,
  userId: UUID,
  codeChallenge: String,
  scope: String)

/** OAuth の認可コードと、認可で発行した API キー。 */
final class OAuthStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  def createCode(
    hash: Array[Byte],
    c: OAuthCode,
    expires: Instant
  ): IO[Unit] = tx(
    sql"""insert into oauth_code (code_hash, client_id, redirect_uri, user_id, code_challenge, scope, expires_at)
          values ($hash, ${c.clientId}, ${c.redirectUri}, ${c.userId}, ${c.codeChallenge}, ${c.scope}, $expires)""".update.run *>
      sql"delete from oauth_code where expires_at < now() - interval '1 hour'".update.run.void
  )

  /** 認可コードを使う（有効期限内で、まだ使っていないものだけ。使ったら使用済みにする）。 */
  def consumeCode(
    hash: Array[Byte]
  ): IO[Option[OAuthCode]] = tx(
    sql"""update oauth_code set used_at = now() where code_hash = $hash and used_at is null and expires_at > now()
          returning client_id, redirect_uri, user_id, code_challenge, scope""".query[OAuthCode].option
  )

  /** そのユーザーにそのクライアントが以前に発行したキーを失効させ、新しいキーを登録する。 */
  def replaceKey(
    userId: UUID,
    clientId: String,
    name: String,
    prefix: String,
    hash: Array[Byte],
    scopes: List[String]
  ): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"update api_key set revoked_at = now() where user_id = $userId and oauth_client = $clientId and revoked_at is null".update.run *>
      sql"""insert into api_key (id, user_id, name, prefix, key_hash, scopes, oauth_client)
            values ($id, $userId, $name, $prefix, $hash, ${scopes.toArray}, $clientId)""".update.run.as(id)
  }

  /** そのクライアントが発行したキーを失効させる（なければ何もしない）。 */
  def revokeKey(
    hash: Array[Byte],
    clientId: String
  ): IO[Unit] = tx(
    sql"update api_key set revoked_at = now() where key_hash = $hash and oauth_client = $clientId and revoked_at is null".update.run.void
  )
}
