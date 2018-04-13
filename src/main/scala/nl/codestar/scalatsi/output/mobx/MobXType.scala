package nl.codestar.scalatsi.output.mobx

sealed trait MobXType {
}

case class MobXString() extends MobXType
case class MobXNumber() extends MobXType
case class MobXBoolean() extends MobXType
case class MobXNull() extends MobXType
case class MobXUndefined() extends MobXType

case class MobXArray(inner: MobXType) extends MobXType
case class MobXEntity(name: Option[String], fields: Map[String, MobXType]) extends MobXType
case class MobXUnion(types: Seq[MobXType]) extends MobXType

sealed trait MobXLiteralType extends MobXType
case class MobXLiteralString(value: String) extends MobXLiteralType
case class MobXLiteralNumber(value: BigDecimal) extends MobXLiteralType
case class MobXLiteralBoolean(value: Boolean) extends MobXLiteralType

case class MobXMaybe(inner: MobXType) extends MobXType

case class MobXCompose(name: Option[String], composition: Seq[MobXType]) extends MobXType

case class MobXEnum(name: String, values: Seq[String]) extends MobXType

case class MobXLate(inner: MobXType) extends MobXType

case class MobXFrozen() extends MobXType

case class MobXAlias(name: String, inner: MobXType) extends MobXType

object MobXType {


}