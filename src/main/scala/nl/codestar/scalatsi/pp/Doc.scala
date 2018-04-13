package nl.codestar.scalatsi.pp

sealed trait Doc {
  def <>(rhs: Doc): Doc = H(this, rhs)
  def $$(rhs: Doc): Doc = V(this, rhs)
}



case class V(lhs: Doc, rhs: Doc) extends Doc
case class H(lhs: Doc, rhs: Doc) extends Doc
case class Text(s: String) extends Doc
case class Sep(docs: Seq[Doc]) extends Doc
case class Nest(n: Int, doc: Doc) extends Doc
case class Empty() extends Doc
case class Newline() extends Doc
