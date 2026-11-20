package skea.web

import skea.shared.UserBrief

// sbirds-lib の画面の部品（.st からは skea.web.* として見える）
export sbirds.web.{
  ApiError,
  AppNames,
  Backdrop,
  Dialog,
  DialogReq,
  Dom,
  Fallback,
  Formats,
  Images,
  Observe,
  Opt,
  Person,
  Pwa,
  Toasts
}

/** アイコン（parts/Avatar.st）に出す人. */
def person(u: UserBrief): Person = Person(u.id.toString, u.name, u.avatarUrl)
