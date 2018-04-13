package nl.codestar

import nl.codestar.scalatsi.{DefaultTSTypes, TSIType, TSType, TypeFlattener}
import org.scalatest.{FlatSpec, Matchers}

class TypeFlattenerSpec extends FlatSpec with Matchers with DefaultTSTypes{

  "TypeScriptFlattener" should "extract dependencies recursively and build a topologically sorted list of types" in {

    case class Person(name: String, address: Address)
    case class Address(street: String, number: StreetNumber)
    case class StreetNumber(number: Int)


    implicit val streetNumber : TSType[StreetNumber] = TSType.alias[StreetNumber, Int]("StreetNumber")
    implicit val address: TSIType[Address] = TSType.fromCaseClass[Address]
    implicit val person: TSIType[Person] = TSType.fromCaseClass[Person]

    val flattened = TypeFlattener.flatten(Seq(person.get))

    flattened shouldBe Seq(
      streetNumber.get,
      address.get,
      person.get
    )
  }

}
