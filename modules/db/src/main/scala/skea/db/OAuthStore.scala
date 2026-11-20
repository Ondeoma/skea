package skea.db

import cats.effect.*
import doobie.*

type OAuthCode = sbirds.oauth.OAuthCode
val OAuthCode = sbirds.oauth.OAuthCode

/** OAuth の認可コードと、認可で発行した API キー（表の形は sbirds.oauth.OAuthKeyStore のもの）。 */
final class OAuthStore(xa: Transactor[IO]) extends sbirds.oauth.OAuthKeyStore(xa)
