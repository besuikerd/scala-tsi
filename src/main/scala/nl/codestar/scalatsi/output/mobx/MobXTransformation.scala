package nl.codestar.scalatsi.output.mobx

import nl.codestar.scalatsi.TypescriptType
import nl.codestar.scalatsi.TypescriptType._

object MobXTransformation {
  def transform(tp: TypescriptType): MobXType = tp match {
    case aggregate: TypescriptAggregateType => transformAggregate(aggregate)
    case literal : TSLiteralType[_] => transformLiteral(literal)
    case TSAny => MobXFrozen()
    case TSBoolean => MobXBoolean()
    case TSNever => MobXUndefined()
    case TSNull => MobXNull()
    case TSNumber => MobXNumber()
    case TSString => MobXString()
    case TSUndefined => MobXUndefined()
    case TSVoid => MobXUndefined()
  }

  def transformLiteral(tp: TSLiteralType[_]): MobXType = tp match {
    case TSLiteralString(value) => MobXLiteralString(value)
    case TSLiteralNumber(value) => MobXLiteralNumber(value)
    case TSLiteralBoolean(value) => MobXLiteralBoolean(value)
  }


  def transformAggregate(tp: TypescriptAggregateType): MobXType = tp match {
    case TSAlias(name, underlying) => MobXAlias(name, transform(underlying))
    case TSArray(elementType) => MobXArray(transform(elementType))
    case TSEnum(name, const, entries) => MobXEnum(name, entries.keys.toSeq)
    case TSIndexedInterface(indexName, indexType, valueType) => ??? //TODO
    case TSInterfaceIndexed(name, indexName, indexType, valueType) => ??? //TODO
    case TSInterface(name, members) => MobXEntity(Some(name), members.mapValues(transform))
    case TSIntersection(of) => MobXCompose(None, of.map(transform))
    case TSTuple(of) =>
      val tpes = of.zipWithIndex.map {
        case (t, i) => MobXEntity(None, Map(s"_$i" -> transform(t)))
      }
      MobXCompose(None, tpes)
    case TSUnion(of) =>
      MobXUnion(of.map(transform))
  }
}
