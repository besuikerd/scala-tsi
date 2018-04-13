package nl.codestar.scalatsi.output.mobx

import nl.codestar.scalatsi.TSType
import org.scalatest.{FlatSpec, Matchers}
import nl.codestar.scalatsi._


class MobXTransformationSpec extends FlatSpec with Matchers with DefaultTSTypes {
  "MobX transformation" should "transform a simple interface" in {
    case class Person(name: String, age: Int)

    val ts = TSType.fromCaseClass[Person]

    val mobx = MobXTransformation.transform(ts.get)

    mobx shouldEqual MobXEntity(Some("IPerson"), Map(
      "name" -> MobXString(),
      "age" -> MobXNumber()
    ))
  }
}
