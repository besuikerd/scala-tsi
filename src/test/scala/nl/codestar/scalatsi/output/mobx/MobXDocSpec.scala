package nl.codestar.scalatsi.output.mobx

import nl.codestar.scalatsi.pp.DocPrinter
import nl.codestar.scalatsi.{DefaultTSTypes, TSType}
import org.scalatest.{FlatSpec, Matchers}

class MobXDocSpec extends FlatSpec with Matchers with DefaultTSTypes{
  "MobXDoc" should "pretty print a simple type definition" in {

    case class Person(name: String, age: Int)

    val ts = TSType.fromCaseClass[Person]

    val mobx = MobXTransformation.transform(ts.get)

    val doc = MobXDoc.toDoc(mobx)

    val serialized = DocPrinter.pp(doc)

    serialized shouldEqual
      """types.entity("IPerson", {
        |  name : types.string,
        |  age : types.number
        |})""".stripMargin
  }
}
