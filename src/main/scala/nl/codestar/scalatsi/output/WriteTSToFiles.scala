package nl.codestar.scalatsi.output

import java.io.FileWriter

import nl.codestar.scalatsi.TypescriptType.TypescriptNamedType
import nl.codestar.scalatsi.TypescriptTypeSerializer
import nl.codestar.scalatsi.output.mobx.MobXEmitter

object WriteTSToFiles {
  type Emitter = Seq[TypescriptNamedType] => String
  val emitters: Map[String, Emitter] = Map(
    "typescript" -> TypescriptTypeSerializer.emits _,
    "mobx" -> MobXEmitter.emit _
  ).withDefault(_ => TypescriptTypeSerializer.emits _)

  def write(options: OutputOptions)(types: Seq[TypescriptNamedType]): Unit = {
    val output = emitters(options.outputFormat)(types)
    if (!options.targetFile.exists()) {
      options.targetFile.createNewFile()
    }
    val writer = new FileWriter(options.targetFile)
    writer.write(output)
    writer.close()
  }
}
