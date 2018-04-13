package nl.codestar.scalatsi.pp

import scala.language.implicitConversions


package object dsl {

  implicit def string2Doc(s: String): Doc = text(s)

  def optional(doc: Option[Doc]): Doc = doc.getOrElse(empty)

  def text(s: String): Doc = Text(s)
  def sep(docs: Seq[Doc]): Doc = Sep(docs)
  def sep2(docs: Doc*): Doc = sep(docs.toList)

  def nest(n: Int, doc: Doc): Doc = Nest(n, doc)
  def group(docs: Seq[Doc], sep: Doc): Doc =
    if(docs.isEmpty)
      empty
    else
      docs.reverse.reduceLeft[Doc]{
        case (acc, cur) => cur <> sep <> acc
      }

  def quoted(d: Doc, quote: Doc): Doc = quote <> d <> quote
  def quoted(d: Doc): Doc = quoted(d, "\"")
  val newline: Doc = Newline()
  val empty = Empty()
}
