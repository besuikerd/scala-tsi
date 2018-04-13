package nl.codestar.scalatsi.plugin

import sbt.Keys._
import sbt._
import sbt.info.BuildInfo

object TypescriptGenPlugin extends AutoPlugin {
  object autoImport {
    val generateTypescript =
      TaskKey[Unit]("generateTypescript", "Generate typescript this project")
    val generateTypescriptGeneratorApplication = TaskKey[Seq[File]](
      "generateTypescriptGeneratorApplication",
      "Generate an application that will generate typescript from the classes that are configured")

    val typescriptClassesToGenerateFor =
      SettingKey[Seq[String]](
        "typescriptClassesToGenerateFor",
        "Classes to generate typescript interfaces for")
    val typescriptGenerationImports = SettingKey[Seq[String]](
      "typescriptGenerationImports",
      "Additional imports (i.e. your packages so you don't need to prefix your classes)")
    val typescriptOutputFile = SettingKey[File](
      "typescriptOutputFile",
      "File where all typescript interfaces will be written to")
    val typescriptOutputFormat = SettingKey[String](
      "typescriptOutputFormat",
      s"Output to format the value to (typescript/mobx)")
  }

  import autoImport._

  override def trigger = allRequirements

  private val scala_ts_compiler_version = BuildInfo.version

  override lazy val projectSettings = Seq(
    // User settings
    libraryDependencies += "nl.codestar" %% "scala-tsi" % scala_ts_compiler_version,
    typescriptGenerationImports := Seq(),
    typescriptClassesToGenerateFor := Seq(),
    typescriptOutputFile := target.value / "scala-interfaces.ts",
    // Task settings
    generateTypescript := runTypescriptGeneration.value,
    generateTypescriptGeneratorApplication in Compile := createTypescriptGenerationTemplate(
      typescriptGenerationImports.value,
      typescriptClassesToGenerateFor.value,
      sourceManaged.value,
      typescriptOutputFile.value,
      typescriptOutputFormat.value),
    sourceGenerators in Compile += generateTypescriptGeneratorApplication in Compile,
  )

  def createTypescriptGenerationTemplate(
      imports: Seq[String],
      typesToGenerate: Seq[String],
      sourceManaged: File,
      typescriptOutputFile: File,
      outputFormat: String): Seq[File] = {
    val targetFile = sourceManaged / "nl" / "codestar" / "scala" / "ts" / "generator" / "ApplicationTypescriptGeneration.scala"

    val toWrite: String = txt
      .generateTypescriptApplicationTemplate(
        imports,
        typesToGenerate,
        typescriptOutputFile.getAbsolutePath,
        outputFormat
      )
      .body
      .stripMargin

    IO.write(targetFile, toWrite)
    Seq(targetFile)
  }

  def runTypescriptGeneration: Def.Initialize[Task[Unit]] =
    (runMain in Compile)
      .toTask(" nl.codestar.scalatsi.generator.ApplicationTypescriptGeneration")
      .dependsOn(generateTypescriptGeneratorApplication in Compile)
}
