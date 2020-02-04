## api4s - HTTP API in Scala
### Description

Document your HTTP API with [swagger](https://swagger.io) and generate Scala code from it.

Uses [http4s](https://github.com/http4s/http4s) and [circe](https://github.com/circe/circe) for autogenerated code.

### Usage

The simplest way to use this library is via sbt plugin. Add following line to `project/plugins.sbt`:

```scala
addSbtPlugin("com.github.IndiscriminateCoding" % "api4s-sbt" % "0.2.5")
```
Also you need to enable plugin usage in `build.sbt`:
```scala
lazy val root = (project in file("."))
  .enablePlugins(Api4s)
  .settings(
    libraryDependencies += "com.github.IndiscriminateCoding" %% "api4s-core" % "0.2.5",
    api4sSources := Seq(Api4s.Src(
      file = sourceDirectory.value / "main" / "swagger" / "my-specification.yaml",
      pkg = "com.example",
      server = true,
      client = true
    ))
  )
```
At this point you can just compile your project and see what is generated by plugin.

Detailed example is [here](./example).

### Limitations
* No OpenAPI 3.0 support at this moment; only swagger is supported
* Specifications in JSON format isn't supported; use YAML instead

