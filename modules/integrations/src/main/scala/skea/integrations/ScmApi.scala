package skea.integrations

import cats.effect.*
import io.circe.{ACursor, Json}
import org.http4s.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.headers.Authorization

import java.time.{Instant, OffsetDateTime}

/** 各サービスの REST API で最近のコミットを取得する（Webhook を設定できない環境向けのポーリング）。 */
final class ScmApi(http: Client[IO]) {

  /** API の URL（未指定ならサービスの既定。Forgejo は Web の URL から作る）。 */
  def apiBase(p: ScmProvider, webUrl: String, apiUrl: Option[String]): String =
    apiUrl.map(_.stripSuffix("/")).getOrElse(p match {
      case ScmProvider.GitHub => "https://api.github.com"
      case ScmProvider.GitLab =>
        val u = Uri.unsafeFromString(webUrl)
        s"${u.scheme.fold("https")(_.value)}://${u.authority.fold("gitlab.com")(_.toString)}/api/v4"
      case ScmProvider.Bitbucket => "https://api.bitbucket.org/2.0"
      case ScmProvider.Forgejo =>
        val u = Uri.unsafeFromString(webUrl)
        s"${u.scheme.fold("https")(_.value)}://${u.authority.fold("")(_.toString)}/api/v1"
    })

  private def get(url: String, auth: Header.ToRaw): IO[Json] =
    http.expect[Json](Request[IO](
      Method.GET,
      Uri.unsafeFromString(url)
    ).putHeaders(auth, "Accept" -> "application/json", "User-Agent" -> "skea"))

  private def s(c: ACursor, k: String) = c.get[String](k).getOrElse("")

  /** name は owner/repo（Bitbucket は workspace/repo、GitLab はグループを含むパス）。 */
  def recentCommits(p: ScmProvider,
                    webUrl: String,
                    apiUrl: Option[String],
                    name: String,
                    branch: Option[String],
                    token: String): IO[List[ScmCommit]] = {
    val base = apiBase(p, webUrl, apiUrl)
    val enc = java.net.URLEncoder.encode(_: String, "UTF-8")
    p match {
      case ScmProvider.GitHub | ScmProvider.Forgejo =>
        val auth: Header.ToRaw =
          if p == ScmProvider.GitHub then Authorization(Credentials.Token(AuthScheme.Bearer, token))
          else ("Authorization" -> s"token $token")
        val q = branch.fold("")(b => s"&sha=${enc(b)}")
        val per = if p == ScmProvider.GitHub then "per_page" else "limit"
        get(s"$base/repos/$name/commits?$per=50$q", auth).map(_.asArray.toList.flatten.map { j =>
          val c = j.hcursor
          ScmCommit(
            s(c, "sha"),
            c.downField("commit").get[String]("message").getOrElse("").trim,
            s(c, "html_url"),
            c.downField("commit").downField("author").get[String]("name").getOrElse(""),
            scala.util.Try(
              Instant.parse(c.downField("commit").downField("author").get[String]("date").getOrElse(""))
            ).getOrElse(Instant.now()),
            branch
          )
        })
      case ScmProvider.GitLab =>
        val q = branch.fold("")(b => s"&ref_name=${enc(b)}")
        get(
          s"$base/projects/${enc(name)}/repository/commits?per_page=50$q",
          ("PRIVATE-TOKEN" -> token)
        ).map(_.asArray.toList.flatten.map { j =>
          val c = j.hcursor
          ScmCommit(
            s(c, "id"),
            s(c, "message").trim,
            s(c, "web_url"),
            s(c, "author_name"),
            scala.util.Try(OffsetDateTime.parse(s(c, "committed_date")).toInstant).getOrElse(Instant.now()),
            branch
          )
        })
      case ScmProvider.Bitbucket =>
        val path = branch.fold(s"$base/repositories/$name/commits")(b => s"$base/repositories/$name/commits/${enc(b)}")
        get(s"$path?pagelen=50", Authorization(Credentials.Token(AuthScheme.Bearer, token))).map(
          _.hcursor.downField("values").as[List[Json]].getOrElse(Nil).map { j =>
            val c = j.hcursor
            ScmCommit(
              s(c, "hash"),
              s(c, "message").trim,
              c.downField("links").downField("html").get[String]("href").getOrElse(""),
              c.downField("author").get[String]("raw").getOrElse(""),
              scala.util.Try(OffsetDateTime.parse(s(c, "date")).toInstant).getOrElse(Instant.now()),
              branch
            )
          }
        )
    }
  }

  /** 最近更新されたリポジトリ（owner/repo）。owner を指定するとその組織・ユーザー・グループのものだけ（一括監視のポーリング）。 */
  def recentRepos(p: ScmProvider,
                  webUrl: String,
                  apiUrl: Option[String],
                  owner: Option[String],
                  token: String,
                  limit: Int = 20): IO[List[String]] = {
    val base = apiBase(p, webUrl, apiUrl)
    val enc = java.net.URLEncoder.encode(_: String, "UTF-8")
    val names: IO[List[String]] = p match {
      case ScmProvider.GitHub =>
        val auth = Authorization(Credentials.Token(AuthScheme.Bearer, token))
        get(
          owner.fold(s"$base/user/repos?sort=pushed&per_page=$limit")(o =>
            s"$base/orgs/${enc(o)}/repos?sort=pushed&per_page=$limit"
          ),
          auth
        )
          .handleErrorWith(e =>
            owner.fold(IO.raiseError(e))(o => get(s"$base/users/${enc(o)}/repos?sort=pushed&per_page=$limit", auth))
          )
          .map(_.asArray.toList.flatten.flatMap(_.hcursor.get[String]("full_name").toOption))
      case ScmProvider.Forgejo =>
        val auth: Header.ToRaw = "Authorization" -> s"token $token"
        get(owner.fold(s"$base/user/repos?limit=$limit")(o => s"$base/orgs/${enc(o)}/repos?limit=$limit"), auth)
          .map(_.asArray.toList.flatten.flatMap(_.hcursor.get[String]("full_name").toOption))
      case ScmProvider.GitLab =>
        val q =
          owner.fold(s"$base/projects?membership=true")(o => s"$base/groups/${enc(o)}/projects?include_subgroups=true")
        get(s"$q&order_by=last_activity_at&per_page=$limit", ("PRIVATE-TOKEN" -> token))
          .map(_.asArray.toList.flatten.flatMap(_.hcursor.get[String]("path_with_namespace").toOption))
      case ScmProvider.Bitbucket =>
        val auth = Authorization(Credentials.Token(AuthScheme.Bearer, token))
        get(
          owner.fold(s"$base/repositories?role=member&sort=-updated_on&pagelen=$limit")(o =>
            s"$base/repositories/${enc(o)}?sort=-updated_on&pagelen=$limit"
          ),
          auth
        )
          .map(_.hcursor.downField(
            "values"
          ).as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("full_name").toOption))
    }
    names.map(_.take(limit))
  }
}
