package nl.codestar.scalatsi.output.mobx

import nl.codestar.scalatsi.pp.Doc
import nl.codestar.scalatsi.pp.dsl._

object MobXDoc {
  def toDoc(mobXType: MobXType): Doc = mobXType match {
    case MobXString() => types("string")
    case MobXNumber() => types("number")
    case MobXBoolean() => types("boolean")
    case MobXNull() => types("null")
    case MobXUndefined() => types("undefined")
    case MobXArray(inner) => types("array", toDoc(inner))

    case MobXEntity(name, fields) =>
      "types.entity(" <> nameArg(name) <> "{" <> newline <>
        nest(2,
          group(
            fields.map((typeArg _).tupled).toSeq, "," <> newline
          )
        ) <> newline <> "})"

    case MobXUnion(types) =>
      "types.union(" <>
        nest(2,
          group(
            types.map(toDoc), ", " <> newline
          )
        ) <> newline <> ")"

    case MobXLiteralString(value) => literal(value.toString)
    case MobXLiteralBoolean(value) => literal(value.toString)
    case MobXLiteralNumber(value) => literal(value.toString())
    case MobXMaybe(inner) => types("maybe", toDoc(inner))

    case MobXCompose(name, composition) =>
      "types.compose(" <> nameArg(name) <>
        nest(2,
          group (
            composition.map(toDoc),
            ", "
          )
        ) <> newline <> ")"


    case MobXEnum(name, values) =>
      "types.enumeration(" <> nameArg(Some(name)) <> "[" <> group(values.map(text), ", " ) <> "])"

    case MobXLate(inner) => types("late", toDoc(inner))
    case MobXFrozen() => types("frozen")

    case MobXAlias(name, inner) => name
  }

  def nameArg(name: Option[String]): Doc = optional(name.map(quoted(_) <> ", "))
  def typeArg(key: String, value: MobXType): Doc = key <> " : " <> toDoc(value)
  def types(name: String, innerTypes: Doc*): Doc = "types." <> name <>
    (if(innerTypes.nonEmpty)
      "(" <> sep(innerTypes) <> ")"
    else
      empty
    )
  def literal(value: Doc): Doc = types("literal", value)
}
