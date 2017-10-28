import sbt.Keys.sourceGenerators


lazy val root = (project in file("."))
  .enablePlugins(nl.codestar.scala.ts.plugin.TypescriptGenPlugin)
  .settings(
    version := "0.1",
    scalaVersion := "2.12.4",
    typescriptGenerationImports in Compile := Seq("domain._"),
    typescriptClassesToGenerateFor in Compile := Seq("Foo"),
    typescriptOutputFile in Compile := "IFoo.ts",
    libraryDependencies += "nl.codestar" %% "scala-tsi" % "0.1-SNAPSHOT"
  )
