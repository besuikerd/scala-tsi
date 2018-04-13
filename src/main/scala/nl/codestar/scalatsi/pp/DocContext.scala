package nl.codestar.scalatsi.pp

class DocContext(
  var h: Int,
  var v: Int,
  var hOffset: Int,
  val builder: StringBuilder,
  val hLimit: Int = 80
)