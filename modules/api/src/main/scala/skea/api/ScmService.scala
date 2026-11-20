package skea.api

import sbirds.db.*
import sbirds.observe.Log

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skea.auth.SecretBox
import skea.core.*
import skea.db.*
import skea.integrations.*
import skea.shared.*

import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.concurrent.duration.*

/** ソースコード管理との連携（Webhook の受信・ポーリング）。コミットメッセージのタスクIDでタスクにリンクする。 */
final class ScmService(integ: IntegrationStore,
                       tasks: TaskStore,
                       projects: ProjectStore,
                       hub: EventHub,
                       api: ScmApi,
                       box: SecretBox,
                       lock: JobLock) {

  /** 受信した Webhook。リンクしたコミットの数を返す。 */
  def receive(repoId: UUID, headers: Headers, body: Array[Byte]): Res[Int] =
    integ.repo(repoId).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.ScmRepoNotFound)))
      case Some(r) =>
        ScmProvider.parse(r.provider) match {
          case None => IO.pure(Left(AppError(ErrorCode.ScmProviderUnknown)))
          case Some(p) if !ScmWebhook.verify(p, r.secret, headers, body) =>
            IO.pure(Left(AppError(ErrorCode.ScmSignatureInvalid)))
          case Some(p) if !ScmWebhook.isPush(p, headers) => IO.pure(Right(0))
          case Some(p) =>
            ScmWebhook.commits(p, new String(body, StandardCharsets.UTF_8)) match {
              case Left(e) => IO.pure(Left(AppError(ErrorCode.InvalidInput, e)))
              case Right(cs) => Integrations.called("scm.webhook", "ok") *> linkAll(r, cs).map(Right(_))
            }
        }
    }

  def linkAll(r: RepoRow, commits: List[ScmCommit]): IO[Int] =
    projects.find(r.projectId).flatMap {
      case None => IO.pure(0)
      case Some(p) =>
        // 一括監視で組織名などを指定したときは、その配下のリポジトリだけ
        val owner = Option.when(r.allRepos)(r.name.trim.toLowerCase).filter(_.nonEmpty)
        commits.filter(c => owner.forall(o => c.repo.exists(_.toLowerCase.startsWith(o + "/")))).traverse { c =>
          val keys = TaskKeys.extract(c.message).filter(_.startsWith(p.key + "-"))
          val repo = c.repo.getOrElse(r.name)
          tasks.idsByKeys(p.id, keys).flatMap(_.values.toList.traverse { tid =>
            tasks.linkCommit(tid, r.id, repo, c.sha, c.message, c.url, c.author, c.branch, c.at).flatMap { isNew =>
              if !isNew then IO.pure(0)
              else
                hub.emitSystem(
                  p.id,
                  Some(tid),
                  "scm",
                  EventKind.CommitLinked,
                  Json.obj(
                    "sha" -> c.sha.asJson,
                    "message" -> c.message.take(500).asJson,
                    "url" -> Option(c.url.trim).filter(u =>
                      u.startsWith("https://") || u.startsWith("http://")
                    ).getOrElse("").asJson,
                    "author" -> c.author.asJson,
                    "repo" -> repo.asJson,
                    "branch" -> c.branch.asJson
                  )
                ).as(1)
            }
          }).map(_.sum)
        }.map(_.sum)
    }

  def poll(r: RepoRow): Res[Int] =
    (ScmProvider.parse(r.provider), r.tokenEnc.flatMap(box.decrypt)) match {
      case (Some(p), Some(token)) =>
        (
          NetGuard.check(r.webUrl),
          r.apiUrl.filter(_.trim.nonEmpty).fold(IO.pure(Right(()): Either[String, Unit]))(NetGuard.check)
        ).tupled.flatMap {
          case (Left(m), _) => IO.pure(Left(AppError(ErrorCode.InvalidInput, m)))
          case (_, Left(m)) => IO.pure(Left(AppError(ErrorCode.InvalidInput, m)))
          case _ => pollWith(r, p, token)
        }
      case (None, _) => IO.pure(Left(AppError(ErrorCode.ScmProviderUnknown)))
      case (_, None) => IO.pure(Left(AppError(ErrorCode.ScmTokenMissing)))
    }

  private def pollWith(r: RepoRow, p: ScmProvider, token: String): Res[Int] =
    sbirds.observe.Telemetry.span("scm.poll", "scm.provider" -> r.provider) {
      val commits =
        if !r.allRepos then api.recentCommits(p, r.webUrl, r.apiUrl, r.name, r.branch, token)
        else
          // 一括監視: 最近更新されたリポジトリごとに取得する（取得できないリポジトリは飛ばす）
          api.recentRepos(p, r.webUrl, r.apiUrl, Option(r.name.trim).filter(_.nonEmpty), token).flatMap(_.flatTraverse(
            n =>
              api.recentCommits(p, r.webUrl, r.apiUrl, n, None, token).map(_.map(_.copy(repo = Some(n)))).handleError(
                _ =>
                  Nil
              )
          ))
      commits.attempt.flatMap {
        case Left(e) =>
          Integrations.called("scm.poll", "error") *>
            Log.report(ErrorCode.ScmPollFailed, s"${r.id}: ${e.getMessage}", e) *>
            IO.pure(Left(AppError(ErrorCode.ScmCommitsUnavailable)))
        case Right(cs) => Integrations.called("scm.poll", "ok") *>
            linkAll(r, cs).flatTap(_ => integ.markPolled(r.id, cs.headOption.map(_.sha))).map(Right(_))
      }
    }

  /** トークンを登録したリポジトリを5分ごとに確認する（複数ノードでも1つだけが動く）。 */
  def pollLoop: IO[Nothing] =
    Periodic.run(1.minute, 5.minutes, ErrorCode.ScmPollingFailed)(
      lock.exclusive("scm-poll", 4.minutes)(integ.pollableRepos.flatMap(_.traverse_(r => poll(r).void)))
    )
}
