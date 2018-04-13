package nl.codestar.scalatsi.output.mobx

import nl.codestar.scalatsi.TypeFlattener
import nl.codestar.scalatsi.TypescriptType.TypescriptNamedType
import nl.codestar.scalatsi.pp.dsl._
import nl.codestar.scalatsi.pp.{Doc, DocPrinter}

object MobXEmitter {
  def emit(types: Seq[TypescriptNamedType]): String = {
    val doc = TypeFlattener.flatten(types)
      .foldLeft[Doc](empty)(_ $$ empty $$ toDoc(_))
    DocPrinter.pp(doc)
  }

  def toDoc(tp: TypescriptNamedType): Doc =
    definition(tp) $$ typeDefinition(tp) $$ interfaceDefinition(tp)


  def definition(tp: TypescriptNamedType): Doc =
    "export const " <> tp.name <> " = " <> MobXDoc.toDoc(MobXTransformation.transform(MobXTransformation.aliasNamedTypes(tp)))

  def typeDefinitionName(tp: TypescriptNamedType): String = s"${tp.name}Type"

  def typeDefinition(tp: TypescriptNamedType): Doc =
    "export type " <> typeDefinitionName(tp) <> " Type = typeof " <> tp.name <> ".Type"

  def interfaceDefinitionName(tp: TypescriptNamedType): String = s"I${tp.name}"

  def interfaceDefinition(tp: TypescriptNamedType): Doc =
    "export interface " <> interfaceDefinitionName(tp) <> " extends " <> typeDefinitionName(tp) <> " {}"
}
