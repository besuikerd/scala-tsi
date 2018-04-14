package nl.codestar.scalatsi.doc

import nl.codestar.scalatsi.pp.v2._
import nl.codestar.scalatsi.pp.v2.dsl._
import org.scalatest.{FlatSpec, Matchers}

class DocSpec extends FlatSpec with Matchers{

  object SpecLang extends SpecDoc
  implicit object Lang extends Normalized()(SpecLang)
  import Lang._

  val doc =
    group(
      (text("Hi") <> line <> text("you")) <> text("!!!")
    )

  val doc2 = group(text("this") <>
    nest(Indent(9), (line <> group(text("takes") <> line <> text("four")))) <>
    line <> text("lines"))

  val result = Doc.run(doc2, 15)

  println(result)

}
