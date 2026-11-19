package skea.api

import cats.effect.*
import skea.core.*
import skea.db.*
import sbirds.shared.AppError

import java.util.UUID

/** 権限の確認（ルートとサービスで共通）。見えないものは 404、権限が足りなければ 403。 */
final class Access2(
  projects: ProjectStore,
  tasks: TaskStore) {

  def project(
    s: AuthSession,
    pid: UUID,
    need: Role
  ): Res[Role] =
    projects.role(pid, s).map {
      case None => Left(AppError(ErrorCode.ProjectNotFound))
      // ヘルプデスクの"ユーザ"には、エージェント向けのものはあることも知らせない
      case Some(r) if !r.isAgent && need.isAgent => Left(AppError(ErrorCode.ProjectNotFound))
      case Some(r) if r < need =>
        Left(AppError(if need == Role.Admin then ErrorCode.ProjectAdminRequired else ErrorCode.ProjectEditRequired))
      case Some(r) => Right(r)
    }

  /** id は UUID またはタスクID（SKEA-12）。 */
  def task(
    s: AuthSession,
    id: String,
    need: Role
  ): Res[(TaskCore, Role)] =
    (uuidOpt(id) match {
      case Some(u) => tasks.core(u)
      case None => tasks.coreByKey(id)
    }).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.TaskNotFound)))
      case Some(t) => project(s, t.projectId, need).map {
          case Left(e) if e.status == 404 => Left(AppError(ErrorCode.TaskNotFound))
          case other => other.map(t -> _)
        }
    }
}
