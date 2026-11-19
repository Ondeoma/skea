package skea.api

import sbirds.observe.Log

import cats.effect.*
import cats.syntax.all.*
import skea.core.*
import skea.db.*
import sbirds.mail.{Mail, Mailer}
import skea.shared.*

import java.util.UUID
import scala.concurrent.duration.*

/** 通知（画面内・メール・PWA のプッシュ通知・連携していればチャット（stui））。 */
final class NotifyService(activity: ActivityStore,
                          projects: ProjectStore,
                          users: UserStore,
                          help: HelpStore,
                          sessions: SessionAuth,
                          cfg: ApiConfig,
                          chat: Option[ChatNotifier] = None,
                          push: Option[sbirds.push.PushService] = None) {

  /**
   * users（操作した本人を除く）に通知する。そのプロジェクトを閲覧できない人には送らない。
   * ヘルプデスクの"ユーザ"には、そのタスクが見えるときの決まった種類の通知だけを送り（内部コメント internal は送らない）、リンクは窓口の画面にする。
   */
  def send(kind: String,
           to: Set[UUID],
           actor: Option[UUID],
           projectId: UUID,
           taskId: Option[UUID],
           commentId: Option[UUID],
           title: String,
           body: String,
           link: String,
           mailDelay: Int = 0,
           dedupe: Option[String] = None,
           internal: Boolean = false): IO[Unit] = {
    val targets = (to -- actor.toSet).toList
    IO.whenA(targets.nonEmpty)(sbirds.observe.Telemetry.span("notify.send", "kind" -> kind) {
      actor.flatTraverse(a => users.find(a).map(_.map(_.name))).flatMap { actorName =>
        def mailBody(link: String) = List(
          actorName.fold("")(n => s"$n さんの操作です。\n\n"),
          title,
          "",
          body.take(2000),
          "",
          s"${cfg.publicUrl}$link",
          "",
          "-- ",
          "skea（通知の設定は画面右上のメニュー →「個人設定」で変えられます）"
        ).mkString("\n")
        def deliver(uid: UUID, link: String): IO[Unit] =
          activity.notify(uid, kind, Some(projectId), taskId, commentId, actor, title, body) *>
            activity.enqueueMail(
              Some(uid),
              None,
              Some(projectId),
              s"[skea] $title",
              mailBody(link),
              dedupe.map(d => s"$d:$uid"),
              mailDelay
            ) *>
            sbirds.observe.Metrics.count("skea.notifications.sent", "kind" -> kind) *>
            // この端末で受け取る設定（PWA のプッシュ通知）にしていれば、ブラウザを閉じていても知らせる（裏で送る）
            push.traverse_(_.send(
              uid,
              sbirds.push.PushMessage(
                title,
                actorName.fold(body)(n => s"$n さん: $body").take(500),
                link,
                Some(s"${taskId.getOrElse(projectId)}:$kind")
              )
            ).start.void) *>
            // チャットへは裏で送る（遅れや失敗で操作を止めない）
            chat.traverse_(_.send(uid, kind, title, actorName.fold(body)(n => s"$n さん: $body"), link).start.void)
        targets.traverse_ { uid =>
          // そのプロジェクトを閲覧できる有効なユーザーだけに送る（"ユーザ"は、そのタスクが見えるときだけ）
          sessions.sessionFor(uid).flatMap(_.flatTraverse(s => projects.role(projectId, s))).flatMap {
            case None => IO.unit
            case Some(r) if !r.isAgent =>
              val allowed =
                if internal || !NotifyService.requesterKinds(kind) then IO.pure(false)
                else taskId.fold(IO.pure(false))(help.canSee(_, uid))
              allowed.ifM(deliver(uid, NotifyService.helpLink(link)), IO.unit)
            case Some(_) => deliver(uid, link)
          }
        }
      }
    }).handleErrorWith(e => Log.report(ErrorCode.NotifyFailed, "", e))
  }

  /** メールの送信ワーカー（30秒ごと）。送信時点でまだ閲覧できるかを確かめる。 */
  def mailLoop(mailer: Option[Mailer]): IO[Nothing] =
    Periodic.run(20.seconds, 30.seconds, ErrorCode.MailWorkerFailed) {
      mailer match {
        case None => activity.processMail(50)(_ => IO.pure(OutboxResult.Skip("SMTP が設定されていません")))
        case Some(m) =>
          activity.processMail(50) { row =>
            (row.userId, row.toAddress) match {
              case (Some(uid), _) =>
                activity.mailTarget(uid, None).flatMap {
                  case None => IO.pure(OutboxResult.Skip("宛先のユーザーが無効か、メールアドレスがありません"))
                  case Some((addr, name)) =>
                    val stillVisible = row.projectId.fold(IO.pure(true))(pid =>
                      sessions.sessionFor(uid).flatMap(_.fold(IO.pure(false))(s =>
                        projects.role(pid, s).map(_.isDefined)
                      ))
                    )
                    stillVisible.ifM(
                      sbirds.observe.Telemetry.span("mail.send")(m.send(Mail(
                        addr,
                        name,
                        row.subject,
                        row.body
                      ))).as(OutboxResult.Sent)
                        .flatTap(_ => sbirds.observe.Metrics.count("skea.mails.sent")),
                      IO.pure(OutboxResult.Skip("宛先がプロジェクトを閲覧できなくなりました"))
                    )
                }
              case (None, Some(addr)) => m.send(Mail(addr, addr, row.subject, row.body)).as(OutboxResult.Sent)
              case _ => IO.pure(OutboxResult.Skip("宛先がありません"))
            }
          }
      }
    }
}

object NotifyService {

  /** ヘルプデスクの"ユーザ"に送る通知の種類（担当・期限・自動化・項目の更新などのエージェント向けのものは送らない）。 */
  val requesterKinds: Set[String] =
    Set(NotificationKind.Comment, NotificationKind.Status, NotificationKind.Mention, NotificationKind.Shared)

  /** エージェントの画面のリンク（/tasks/KEY…）を、"ユーザ"の窓口の画面（/help/t/KEY…）にする。 */
  def helpLink(link: String): String =
    if link.startsWith("/tasks/") then "/help/t/" + link.stripPrefix("/tasks/") else link
}
