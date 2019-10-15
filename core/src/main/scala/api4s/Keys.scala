package api4s

import cats.effect.SyncIO
import io.chrisdavenport.vault.{ Key, Vault }

object Keys {
  val operationId: Key[String] = Key.newKey[SyncIO, String].unsafeRunSync()

  def operationId(id: String): Vault = Vault.empty.insert(operationId, id)
}
