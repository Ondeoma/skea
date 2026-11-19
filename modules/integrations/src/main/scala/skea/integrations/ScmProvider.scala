package skea.integrations

import java.time.Instant

/** リポジトリのコミット（プロバイダによらない形）。repo はリポジトリの名前（owner/repo。分かる場合）。 */
final case class ScmCommit(sha: String,
                           message: String,
                           url: String,
                           author: String,
                           at: Instant,
                           branch: Option[String],
                           repo: Option[String] = None)

enum ScmProvider {
  case GitHub, GitLab, Bitbucket, Forgejo
}

object ScmProvider {
  def parse(s: String): Option[ScmProvider] = s.trim.toLowerCase match {
    case "github" => Some(GitHub)
    case "gitlab" => Some(GitLab)
    case "bitbucket" => Some(Bitbucket)
    case "forgejo" | "gitea" | "codeberg" => Some(Forgejo)
    case _ => None
  }
  extension (p: ScmProvider) {
    def asString: String = p match {
      case GitHub => "github"
      case GitLab => "gitlab"
      case Bitbucket => "bitbucket"
      case Forgejo => "forgejo"
    }
  }
}
