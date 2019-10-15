package api4s.codegen.emitter

import api4s.codegen.ast._

object Attributes {
  def apply(pkg: String, api: Api): String = {
    val items = api.endpoints.values.flatMap(_.values).map { e =>
      s"""  val ${e.name.get}: Vault = Keys.operationId("${e.name.get}")"""
    }.toList

    List(
      s"package $pkg",
      "",
      "import api4s.Keys",
      "import io.chrisdavenport.vault.Vault",
      "",
      "object Attributes {",
      items.mkString("\n"),
      "}"
    ).mkString("\n")
  }
}
