package skea.core

import java.util.UUID

/** UUID として読めれば Some（読めなければ None）。 */
def uuidOpt(s: String): Option[UUID] = scala.util.Try(UUID.fromString(s.trim)).toOption

/**
 * プロジェクトに対する権限。問い合わせ(Requester) < 閲覧(View) < 編集(Edit) < 管理(Admin)。
 * Requester はヘルプデスクの"ユーザ"（問い合わせする側）で、閲覧以上を求める API はすべて使えない（自分に見えるタスクだけを /api/help で扱う）。
 * 閲覧以上のメンバーが"エージェント"（回答する側）。
 */
enum Role:
  case Requester, View, Edit, Admin

object Role:
  given Ordering[Role] = Ordering.by(_.ordinal)

  def parse(s: String): Option[Role] = s.trim.toLowerCase match
    case "requester" => Some(Requester)
    case "view"      => Some(View)
    case "edit"      => Some(Edit)
    case "admin"     => Some(Admin)
    case _           => None

  extension (r: Role)
    def asString: String = r match
      case Requester => "requester"
      case View  => "view"
      case Edit  => "edit"
      case Admin => "admin"
    def >=(o: Role): Boolean = r.ordinal >= o.ordinal
    def <(o: Role): Boolean  = r.ordinal < o.ordinal
    /** エージェント（閲覧以上）か。 */
    def isAgent: Boolean = r >= View

/** APIキーのスコープ。 */
enum Scope:
  case Read, Write, Admin

object Scope:
  def parse(s: String): Option[Scope] = s.trim.toLowerCase match
    case "read"  => Some(Read)
    case "write" => Some(Write)
    case "admin" => Some(Admin)
    case _       => None
  extension (s: Scope)
    def asString: String = s match
      case Read  => "read"
      case Write => "write"
      case Admin => "admin"

final case class User(id: UUID, sub: String, name: String, email: Option[String], avatarUrl: Option[String], isAdmin: Boolean,
    disabled: Boolean, timezone: String)
final case class Group(id: UUID, name: String)

/** 認証手段。 */
enum AuthVia:
  case Session
  /** APIキー。scopes はキーのスコープ、projects はキーで操作できるプロジェクト（None なら制限なし）。mcp は MCP 経由の呼び出し。 */
  case ApiKey(id: UUID, scopes: Set[Scope], projects: Option[Set[UUID]], mcp: Boolean = false)
  /** 自動化のルールの実行（ルールの実行ユーザーとして動く）。 */
  case Automation(ruleId: UUID)

/** 認証済みの利用者。isAdmin はシステム管理者（ADMIN_GROUPS の所属、または画面で管理者にしたユーザー）。 */
final case class AuthSession(user: User, groups: List[Group], isAdmin: Boolean, via: AuthVia = AuthVia.Session):

  /** 更新系の操作ができるか（read スコープだけのキーは不可）。 */
  def canWrite: Boolean = via match
    case AuthVia.ApiKey(_, scopes, _, _) => scopes.contains(Scope.Write) || scopes.contains(Scope.Admin)
    case _                                => true

  /** システム管理の操作ができるか（APIキーは admin スコープが必要）。 */
  def canAdmin: Boolean = isAdmin && (via match
    case AuthVia.ApiKey(_, scopes, _, _) => scopes.contains(Scope.Admin)
    case AuthVia.Automation(_)           => false
    case _                                => true)

  /** キーでこのプロジェクトを操作できるか。 */
  def allowsProject(id: UUID): Boolean = via match
    case AuthVia.ApiKey(_, _, Some(ps), _) => ps.contains(id)
    case _                                  => true

  def viaString: String = via match
    case AuthVia.Session                    => "web"
    case AuthVia.ApiKey(_, _, _, true)      => "mcp"
    case _: AuthVia.ApiKey                  => "api"
    case _: AuthVia.Automation              => "automation"

  def isSession: Boolean = via == AuthVia.Session

  def scopeStrings: List[String] = via match
    case AuthVia.ApiKey(_, s, _, _) => s.toList.map(_.asString).sorted
    case _                          => List("read", "write") ++ Option.when(isAdmin)("admin")

/** プロジェクトでの実効権限。システム管理者は管理、それ以外は本人とグループの権限の最大。 */
object Access:
  def effective(isAdmin: Boolean, userRole: Option[Role], groupRoles: List[Role]): Option[Role] =
    if isAdmin then Some(Role.Admin)
    else (userRole.toList ++ groupRoles).maxOption

  /** APIキーのスコープで権限を絞る（read だけのキーは閲覧に落とす。問い合わせの権限は上げない）。 */
  def limit(s: AuthSession, r: Role): Role =
    if s.canWrite || r < Role.View then r else Role.View

final case class Project(id: UUID, key: String, name: String, description: String, leadId: Option[UUID], archived: Boolean,
    createdAt: java.time.Instant)

object ProjectKey:
  private val Re = "^[A-Z][A-Z0-9]{1,9}$".r
  def valid(k: String): Boolean = Re.matches(k)
