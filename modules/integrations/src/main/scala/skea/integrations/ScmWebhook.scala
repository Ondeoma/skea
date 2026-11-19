package skea.integrations

import io.circe.Json
import io.circe.parser.parse
import org.http4s.*
import sbirds.auth.Crypto

import java.time.{Instant, OffsetDateTime}

/** 受信した Webhook の解析と署名の検証。 */
object ScmWebhook {

  private def header(h: Headers, name: String): Option[String] =
    h.get(org.typelevel.ci.CIString(name)).map(_.head.value)

  /** 署名を検証する。GitHub / Forgejo / Bitbucket は HMAC-SHA256、GitLab は秘密のトークンそのもの。 */
  def verify(p: ScmProvider, secret: String, headers: Headers, body: Array[Byte]): Boolean = {
    val expected = Crypto.hmacHex(secret, body)
    def sig(names: String*): Option[String] =
      names.toList.flatMap(header(headers, _)).headOption.map(_.trim.stripPrefix("sha256="))
    p match {
      case ScmProvider.GitLab => header(headers, "X-Gitlab-Token").exists(Crypto.constantTimeEquals(_, secret))
      case ScmProvider.GitHub => sig("X-Hub-Signature-256").exists(Crypto.constantTimeEquals(_, expected))
      case ScmProvider.Forgejo => sig(
          "X-Forgejo-Signature",
          "X-Gitea-Signature",
          "X-Hub-Signature-256"
        ).exists(Crypto.constantTimeEquals(_, expected))
      case ScmProvider.Bitbucket =>
        sig("X-Hub-Signature-256", "X-Hub-Signature").exists(Crypto.constantTimeEquals(_, expected))
    }
  }

  /** push イベントか（それ以外は無視する。ping などは true を返さない）。 */
  def isPush(p: ScmProvider, headers: Headers): Boolean = p match {
    case ScmProvider.GitHub => header(headers, "X-GitHub-Event").contains("push")
    case ScmProvider.GitLab => header(headers, "X-Gitlab-Event").exists(e => e == "Push Hook" || e == "Tag Push Hook")
    case ScmProvider.Forgejo =>
      (header(headers, "X-Forgejo-Event") orElse header(headers, "X-Gitea-Event")).contains("push")
    case ScmProvider.Bitbucket => header(headers, "X-Event-Key").contains("repo:push")
  }

  private def time(s: String): Instant =
    scala.util.Try(OffsetDateTime.parse(s).toInstant).orElse(scala.util.Try(Instant.parse(s))).getOrElse(Instant.now())

  private def branchOf(ref: Option[String]): Option[String] =
    ref.map(_.stripPrefix("refs/heads/")).filterNot(_.startsWith("refs/tags/"))

  /** push の本文のリポジトリ名（owner/repo）。 */
  def repoName(p: ScmProvider, j: Json): Option[String] = {
    val c = j.hcursor
    (p match {
      case ScmProvider.GitLab => c.downField("project").get[String]("path_with_namespace")
      case _ => c.downField("repository").get[String]("full_name")
    }).toOption.filter(_.nonEmpty)
  }

  /** push の本文からコミットを取り出す。 */
  def commits(p: ScmProvider, body: String): Either[String, List[ScmCommit]] =
    parse(body).left.map(_ => "JSON として読めません").map { j =>
      val c = j.hcursor
      val repo = repoName(p, j)
      p match {
        case ScmProvider.Bitbucket =>
          c.downField("push").downField("changes").as[List[Json]].getOrElse(Nil).flatMap { ch =>
            val cc = ch.hcursor
            val branch = cc.downField("new").get[String]("name").toOption
            cc.downField("commits").as[List[Json]].getOrElse(Nil).map { cm =>
              val x = cm.hcursor
              ScmCommit(
                x.get[String]("hash").getOrElse(""),
                x.get[String]("message").getOrElse("").trim,
                x.downField("links").downField("html").get[String]("href").getOrElse(""),
                x.downField("author").downField(
                  "user"
                ).get[String]("display_name").orElse(x.downField("author").get[String]("raw")).getOrElse(""),
                time(x.get[String]("date").getOrElse("")),
                branch,
                repo
              )
            }
          }
        case _ =>
          val branch = branchOf(c.get[String]("ref").toOption)
          c.downField("commits").as[List[Json]].getOrElse(Nil).map { cm =>
            val x = cm.hcursor
            ScmCommit(
              x.get[String]("id").getOrElse(""),
              x.get[String]("message").getOrElse("").trim,
              x.get[String]("url").getOrElse(""),
              x.downField("author").get[String]("name").getOrElse(""),
              time(x.get[String]("timestamp").getOrElse("")),
              branch,
              repo
            )
          }
      }
    }.map(_.filter(_.sha.nonEmpty))
}
