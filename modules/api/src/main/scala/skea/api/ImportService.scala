package skea.api

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.{Locale, UUID}

/** CSV・Jira からの取り込み（名前でステータス・担当者・ラベル・スプリントを探す。親子は ref でつなぐ）。 */
final class ImportService(projects: ProjectStore, users: UserStore, svc: TaskService, cfg: ApiConfig) {

  private val jiraDate =
    List("d/MMM/yy h:mm a", "d/MMM/yy H:mm", "d/MMM/yy", "yyyy/MM/dd", "yyyy-MM-dd", "M/d/yyyy", "yyyy年M月d日")
      .map(p => DateTimeFormatter.ofPattern(p, Locale.ENGLISH))

  /** 日付（2026-11-15、2026/11/15、Jira の 15/Nov/26 10:00 AM など。時刻は捨てる）。 */
  def date(s: String): Option[LocalDate] = {
    val t = s.trim
    if t.isEmpty then None
    else
      scala.util.Try(LocalDate.parse(t.take(10))).toOption.orElse(
        jiraDate.iterator.flatMap(f =>
          scala.util.Try(LocalDate.parse(t, f)).toOption.orElse(
            scala.util.Try(java.time.LocalDateTime.parse(t, f).toLocalDate).toOption
          )
        ).nextOption()
      ).orElse(
        // 時刻つきの書式は、日付の部分だけで読む
        jiraDate.iterator.flatMap(f => scala.util.Try(LocalDate.parse(t.takeWhile(_ != ' '), f)).toOption).nextOption()
      )
  }

  /** ステータスの名前を、同じ名前か、名前から推した区分のステータスにする。 */
  private def statusFor(sts: List[StatusDto], name: String): (Option[UUID], Option[String]) = {
    val n = name.trim.toLowerCase
    sts.find(_.name.toLowerCase == n) match {
      case Some(st) => (Some(st.id), None)
      case None if n.isEmpty => (None, None)
      case None =>
        val cat =
          if "完了|終了|done|closed|resolved|fixed|クローズ".r.findFirstIn(n).isDefined then StatusCategory.Done
          else if "待|wait|block|hold|保留|pending".r.findFirstIn(n).isDefined then StatusCategory.Waiting
          else if "進行|対応中|作業中|progress|doing|review|レビュー|開発中".r.findFirstIn(n).isDefined then StatusCategory.InProgress
          else StatusCategory.Todo
        (
          sts.find(_.category == cat).map(_.id),
          Some(s"ステータス「$name」がないため「${sts.find(_.category == cat).map(_.name).getOrElse("")}」にしました")
        )
    }
  }

  def run(op: Op, projectId: UUID, req: ImportReq): Res[ImportResultDto] =
    if req.rows.size > 2000 then IO.pure(Left(AppError(ErrorCode.TemplateImportTooMany)))
    else
      (
        projects.statuses(projectId),
        projects.sprints(projectId, includeClosed = false),
        projects.audience(projectId, cfg.adminGroups),
        users.list(None, includeDisabled = false, 5000)
      ).tupled.flatMap {
        (sts, sprints, audience, all) =>
          val members = audience.map(_.id).toSet
          val warnings = List.newBuilder[String]
          def who(row: Int, v: Option[String]): Option[UUID] = v.map(_.trim).filter(_.nonEmpty).flatMap { name =>
            val n = name.toLowerCase
            all.filter(u => members(u.id)).find(u =>
              u.name.toLowerCase == n || u.email.exists(_.toLowerCase == n)
            ) match {
              case Some(u) => Some(u.id)
              case None => warnings += s"${row}行目: 「$name」はプロジェクトのメンバーにいないため空にしました"; None
            }
          }
          // 親を先に作る（ref が取り込みの中にある親子だけをつなぐ）
          val refs = req.rows.flatMap(_.ref).toSet
          val (parents, children) = req.rows.zipWithIndex.partition((r, _) => !r.parentRef.exists(refs))
          val prepared = (parents ++ children).map {
            (r, i) =>
              {
                val row = i + 1
                val (st, sw) = r.status.fold((None: Option[UUID], None: Option[String]))(statusFor(sts, _))
                sw.foreach(w => warnings += s"${row}行目: $w")
                val sp = r.sprint.map(_.trim).filter(_.nonEmpty).flatMap(n =>
                  sprints.find(_.name == n).map(_.id).orElse {
                    warnings += s"${row}行目: スプリント「$n」がないためバックログに入れました"; None
                  }
                )
                val start = r.startDate.flatMap(d => date(d).orElse { warnings += s"${row}行目: 開始日「$d」を読めません"; None })
                val due = r.dueDate.flatMap(d => date(d).orElse { warnings += s"${row}行目: 期日「$d」を読めません"; None })
                r.priority.filter(p => Priority.parse(p).isEmpty).foreach(p =>
                  warnings += s"${row}行目: 優先度「$p」を読めないため「中」にしました"
                )
                r.taskType.filter(t => TaskType.parse(t).isEmpty).foreach(t =>
                  warnings += s"${row}行目: 種類「$t」を読めないため「タスク」にしました"
                )
                (
                  row,
                  r,
                  st,
                  sp,
                  who(row, r.assignee),
                  who(row, r.reporter),
                  start,
                  due.filter(d => start.forall(s => !d.isBefore(s)))
                )
              }
          }
          val bad = prepared.filter(_._2.title.trim.isEmpty).map(_._1)
          bad.foreach(r => warnings += s"${r}行目: タイトルがないため取り込みません")
          // 警告は行の順に並べる
          def sorted = warnings.result().sortBy(_.takeWhile(_.isDigit).toIntOption.getOrElse(0))
          if req.dryRun then IO.pure(Right(ImportResultDto(prepared.size - bad.size, Map.empty, sorted)))
          else
            prepared.filterNot(p => bad.contains(p._1)).foldLeftM((Map.empty[String, (UUID, String)], 0)) {
              case ((keys, n), (row, r, st, sp, as, rp, start, due)) =>
                for {
                  labels <- r.labels.map(_.trim).filter(_.nonEmpty).distinct.traverse(l =>
                    projects.ensureLabel(projectId, l, None).map(_.id)
                  )
                  desc = r.description.map(_.trim).filter(_.nonEmpty).map(RichText.fromMarkdown)
                  parent = r.parentRef.flatMap(keys.get).map(_._1)
                  res <- svc.create(
                    op,
                    projectId,
                    CreateTaskReq(
                      title = r.title.trim.take(500),
                      description = desc,
                      statusId = st,
                      sprintId = sp,
                      assigneeId = as,
                      reporterId = rp,
                      parentId = parent,
                      startDate = start.map(_.toString),
                      dueDate = due.map(_.toString),
                      labelIds = labels,
                      priority = r.priority.flatMap(Priority.parse),
                      taskType = r.taskType.flatMap(TaskType.parse)
                    ),
                    notify = false
                  )
                } yield res match {
                  case Right(t) => (r.ref.fold(keys)(ref => keys.updated(ref, (t.id, t.key))), n + 1)
                  case Left(f) => warnings += s"${row}行目: ${f.message}"; (keys, n)
                }
            }.map((keys, n) => Right(ImportResultDto(n, keys.view.mapValues(_._2).toMap, sorted)))
      }
}
