package nl.codestar.scalatsi.output.mobx

import nl.codestar.scalatsi.{DefaultTSTypes, TSIType, TSType}
import org.scalatest.{FlatSpec, Matchers}

class MobXEmitterSpec extends FlatSpec with Matchers with DefaultTSTypes{

  "MobX emitter" should "emit a nested case class" in {

    case class Person(name: String, address: Address)
    case class Address(street: String, number: StreetNumber)
    case class StreetNumber(number: Int)

    implicit val streetNumber : TSType[StreetNumber] = TSType.alias[StreetNumber, Int]("StreetNumber")
    implicit val address: TSIType[Address] = TSType.fromCaseClass[Address]
    implicit val person: TSIType[Person] = TSType.fromCaseClass[Person]

    val emitted = MobXEmitter.emit(Seq(person.get))
    println(emitted)
    emitted shouldBe ""
  }

}
