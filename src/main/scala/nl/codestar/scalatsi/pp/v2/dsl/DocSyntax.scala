package nl.codestar.scalatsi.pp.v2.dsl

import nl.codestar.scalatsi.pp.v2.Doc

class DocSyntax[D](val d: D)(implicit impl: Doc[D]) extends AnyRef {
  def <>(other: D): D = impl.concat(d, other)
}
