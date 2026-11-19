package skea.auth

/** skea の APIキー（skea_ で始まる）. */
object ApiKeys extends sbirds.auth.ApiKeys("skea_")

/** skea の秘密の値の暗号化（鍵の salt は skea 固有. 変えると保存済みの値を復号できなくなる）. */
object SecretBox {
  def apply(
    secret: String
  ): sbirds.auth.SecretBox = new sbirds.auth.SecretBox(secret, "skea-secret-box:")
}

type SecretBox = sbirds.auth.SecretBox
