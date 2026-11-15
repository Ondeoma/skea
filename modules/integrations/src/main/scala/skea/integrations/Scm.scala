package skea.integrations

import cats.effect.*
import io.circe.{ACursor, Json}
import io.circe.parser.parse
import org.http4s.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skea.auth.Crypto

import java.time.{Instant, OffsetDateTime}

/** リポジトリのコミット（プロバイダによらない形）。repo はリポジトリの名前（owner/repo。分かる場合）。 */
final case class ScmCommit(sha: String, message: String, url: String, author: String, at: Instant, branch: Option[String],
    repo: Option[String] = None)

enum ScmProvider:
  case GitHub, GitLab, Bitbucket, Forgejo

object ScmProvider:
  def parse(s: String): Option[ScmProvider] = s.trim.toLowerCase match
    case "github"                        => Some(GitHub)
    case "gitlab"                        => Some(GitLab)
    case "bitbucket"                     => Some(Bitbucket)
    case "forgejo" | "gitea" | "codeberg" => Some(Forgejo)
    case _                               => None
  extension (p: ScmProvider)
    def asString: String = p match
      case GitHub    => "github"
      case GitLab    => "gitlab"
      case Bitbucket => "bitbucket"
      case Forgejo   => "forgejo"

/** 受信した Webhook の解析と署名の検証。 */
object ScmWebhook:

  private def header(h: Headers, name: String): Option[String] = h.get(org.typelevel.ci.CIString(name)).map(_.head.value)

  /** 署名を検証する。GitHub / Forgejo / Bitbucket は HMAC-SHA256、GitLab は秘密のトークンそのもの。 */
  def verify(p: ScmProvider, secret: String, headers: Headers, body: Array[Byte]): Boolean =
    val expected = Crypto.hmacHex(secret, body)
    def sig(names: String*): Option[String] = names.toList.flatMap(header(headers, _)).headOption.map(_.trim.stripPrefix("sha256="))
    p match
      case ScmProvider.GitLab => header(headers, "X-Gitlab-Token").exists(Crypto.constantTimeEquals(_, secret))
      case ScmProvider.GitHub => sig("X-Hub-Signature-256").exists(Crypto.constantTimeEquals(_, expected))
      case ScmProvider.Forgejo => sig("X-Forgejo-Signature", "X-Gitea-Signature", "X-Hub-Signature-256").exists(Crypto.constantTimeEquals(_, expected))
      case ScmProvider.Bitbucket => sig("X-Hub-Signature-256", "X-Hub-Signature").exists(Crypto.constantTimeEquals(_, expected))

  /** push イベントか（それ以外は無視する。ping などは true を返さない）。 */
  def isPush(p: ScmProvider, headers: Headers): Boolean = p match
    case ScmProvider.GitHub    => header(headers, "X-GitHub-Event").contains("push")
    case ScmProvider.GitLab    => header(headers, "X-Gitlab-Event").exists(e => e == "Push Hook" || e == "Tag Push Hook")
    case ScmProvider.Forgejo   => (header(headers, "X-Forgejo-Event") orElse header(headers, "X-Gitea-Event")).contains("push")
    case ScmProvider.Bitbucket => header(headers, "X-Event-Key").contains("repo:push")

  private def time(s: String): Instant =
    scala.util.Try(OffsetDateTime.parse(s).toInstant).orElse(scala.util.Try(Instant.parse(s))).getOrElse(Instant.now())

  private def branchOf(ref: Option[String]): Option[String] = ref.map(_.stripPrefix("refs/heads/")).filterNot(_.startsWith("refs/tags/"))

  /** push の本文のリポジトリ名（owner/repo）。 */
  def repoName(p: ScmProvider, j: Json): Option[String] =
    val c = j.hcursor
    (p match
      case ScmProvider.GitLab => c.downField("project").get[String]("path_with_namespace")
      case _                  => c.downField("repository").get[String]("full_name")
    ).toOption.filter(_.nonEmpty)

  /** push の本文からコミットを取り出す。 */
  def commits(p: ScmProvider, body: String): Either[String, List[ScmCommit]] =
    parse(body).left.map(_ => "JSON として読めません").map { j =>
      val c    = j.hcursor
      val repo = repoName(p, j)
      p match
        case ScmProvider.Bitbucket =>
          c.downField("push").downField("changes").as[List[Json]].getOrElse(Nil).flatMap { ch =>
            val cc     = ch.hcursor
            val branch = cc.downField("new").get[String]("name").toOption
            cc.downField("commits").as[List[Json]].getOrElse(Nil).map { cm =>
              val x = cm.hcursor
              ScmCommit(
                x.get[String]("hash").getOrElse(""),
                x.get[String]("message").getOrElse("").trim,
                x.downField("links").downField("html").get[String]("href").getOrElse(""),
                x.downField("author").downField("user").get[String]("display_name").orElse(x.downField("author").get[String]("raw")).getOrElse(""),
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
    }.map(_.filter(_.sha.nonEmpty))

/** 各サービスの REST API で最近のコミットを取得する（Webhook を設定できない環境向けのポーリング）。 */
final class ScmApi(http: Client[IO]):

  /** API の URL（未指定ならサービスの既定。Forgejo は Web の URL から作る）。 */
  def apiBase(p: ScmProvider, webUrl: String, apiUrl: Option[String]): String =
    apiUrl.map(_.stripSuffix("/")).getOrElse(p match
      case ScmProvider.GitHub    => "https://api.github.com"
      case ScmProvider.GitLab    =>
        val u = Uri.unsafeFromString(webUrl)
        s"${u.scheme.fold("https")(_.value)}://${u.authority.fold("gitlab.com")(_.toString)}/api/v4"
      case ScmProvider.Bitbucket => "https://api.bitbucket.org/2.0"
      case ScmProvider.Forgejo   =>
        val u = Uri.unsafeFromString(webUrl)
        s"${u.scheme.fold("https")(_.value)}://${u.authority.fold("")(_.toString)}/api/v1")

  private def get(url: String, auth: Header.ToRaw): IO[Json] =
    http.expect[Json](Request[IO](Method.GET, Uri.unsafeFromString(url)).putHeaders(auth, "Accept" -> "application/json", "User-Agent" -> "skea"))

  private def s(c: ACursor, k: String) = c.get[String](k).getOrElse("")

  /** name は owner/repo（Bitbucket は workspace/repo、GitLab はグループを含むパス）。 */
  def recentCommits(p: ScmProvider, webUrl: String, apiUrl: Option[String], name: String, branch: Option[String], token: String): IO[List[ScmCommit]] =
    val base = apiBase(p, webUrl, apiUrl)
    val enc  = java.net.URLEncoder.encode(_: String, "UTF-8")
    p match
      case ScmProvider.GitHub | ScmProvider.Forgejo =>
        val auth: Header.ToRaw =
          if p == ScmProvider.GitHub then Authorization(Credentials.Token(AuthScheme.Bearer, token)) else ("Authorization" -> s"token $token")
        val q = branch.fold("")(b => s"&sha=${enc(b)}")
        val per = if p == ScmProvider.GitHub then "per_page" else "limit"
        get(s"$base/repos/$name/commits?$per=50$q", auth).map(_.asArray.toList.flatten.map { j =>
          val c = j.hcursor
          ScmCommit(s(c, "sha"), c.downField("commit").get[String]("message").getOrElse("").trim, s(c, "html_url"),
            c.downField("commit").downField("author").get[String]("name").getOrElse(""),
            scala.util.Try(Instant.parse(c.downField("commit").downField("author").get[String]("date").getOrElse(""))).getOrElse(Instant.now()), branch)
        })
      case ScmProvider.GitLab =>
        val q = branch.fold("")(b => s"&ref_name=${enc(b)}")
        get(s"$base/projects/${enc(name)}/repository/commits?per_page=50$q", ("PRIVATE-TOKEN" -> token)).map(_.asArray.toList.flatten.map { j =>
          val c = j.hcursor
          ScmCommit(s(c, "id"), s(c, "message").trim, s(c, "web_url"), s(c, "author_name"),
            scala.util.Try(OffsetDateTime.parse(s(c, "committed_date")).toInstant).getOrElse(Instant.now()), branch)
        })
      case ScmProvider.Bitbucket =>
        val path = branch.fold(s"$base/repositories/$name/commits")(b => s"$base/repositories/$name/commits/${enc(b)}")
        get(s"$path?pagelen=50", Authorization(Credentials.Token(AuthScheme.Bearer, token))).map(
          _.hcursor.downField("values").as[List[Json]].getOrElse(Nil).map { j =>
            val c = j.hcursor
            ScmCommit(s(c, "hash"), s(c, "message").trim, c.downField("links").downField("html").get[String]("href").getOrElse(""),
              c.downField("author").get[String]("raw").getOrElse(""),
              scala.util.Try(OffsetDateTime.parse(s(c, "date")).toInstant).getOrElse(Instant.now()), branch)
          })

  /** 最近更新されたリポジトリ（owner/repo）。owner を指定するとその組織・ユーザー・グループのものだけ（一括監視のポーリング）。 */
  def recentRepos(p: ScmProvider, webUrl: String, apiUrl: Option[String], owner: Option[String], token: String, limit: Int = 20): IO[List[String]] =
    val base = apiBase(p, webUrl, apiUrl)
    val enc  = java.net.URLEncoder.encode(_: String, "UTF-8")
    val names: IO[List[String]] = p match
      case ScmProvider.GitHub =>
        val auth = Authorization(Credentials.Token(AuthScheme.Bearer, token))
        get(owner.fold(s"$base/user/repos?sort=pushed&per_page=$limit")(o => s"$base/orgs/${enc(o)}/repos?sort=pushed&per_page=$limit"), auth)
          .handleErrorWith(e => owner.fold(IO.raiseError(e))(o => get(s"$base/users/${enc(o)}/repos?sort=pushed&per_page=$limit", auth)))
          .map(_.asArray.toList.flatten.flatMap(_.hcursor.get[String]("full_name").toOption))
      case ScmProvider.Forgejo =>
        val auth: Header.ToRaw = "Authorization" -> s"token $token"
        get(owner.fold(s"$base/user/repos?limit=$limit")(o => s"$base/orgs/${enc(o)}/repos?limit=$limit"), auth)
          .map(_.asArray.toList.flatten.flatMap(_.hcursor.get[String]("full_name").toOption))
      case ScmProvider.GitLab =>
        val q = owner.fold(s"$base/projects?membership=true")(o => s"$base/groups/${enc(o)}/projects?include_subgroups=true")
        get(s"$q&order_by=last_activity_at&per_page=$limit", ("PRIVATE-TOKEN" -> token))
          .map(_.asArray.toList.flatten.flatMap(_.hcursor.get[String]("path_with_namespace").toOption))
      case ScmProvider.Bitbucket =>
        val auth = Authorization(Credentials.Token(AuthScheme.Bearer, token))
        get(owner.fold(s"$base/repositories?role=member&sort=-updated_on&pagelen=$limit")(o =>
          s"$base/repositories/${enc(o)}?sort=-updated_on&pagelen=$limit"), auth)
          .map(_.hcursor.downField("values").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("full_name").toOption))
    names.map(_.take(limit))
