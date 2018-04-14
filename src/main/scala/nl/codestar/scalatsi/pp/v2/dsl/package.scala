package nl.codestar.scalatsi.pp.v2

package object dsl {
  import scala.language.implicitConversions

  @inline implicit def docSyntax[D](doc: D)(implicit impl: Doc[D]): DocSyntax[D] = new DocSyntax[D](doc)
}