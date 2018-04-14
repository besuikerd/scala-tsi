package nl.codestar.scalatsi.pp.v2

import nl.codestar.scalatsi.pp.{Done, More, Trampoline}

import scala.annotation.tailrec

trait Doc[D] {
  def text(s: String): D
  def line : D
  def group(doc: D): D
  def concat(left: D, right: D): D
  def nest(indent: Indent, doc: D): D
  def pretty(width: Width, doc: D): Layout
  def nil: D = text("")
}

object Doc {
  def run[D](doc: D, width: Int)(implicit impl: Doc[D]): String = impl.pretty(Width(width), doc).value
}

case class SpecParams(
  indent: Indent,
  width: Width,
  horizontal: Horizontal,
  position: Position,
  remaining: Remaining
)

object SpecParams {
  @inline def indent(implicit specParams: SpecParams): Indent = specParams.indent
  @inline def width(implicit specParams: SpecParams): Width = specParams.width
  @inline def horizontal(implicit specParams: SpecParams): Horizontal = specParams.horizontal
  @inline def position(implicit specParams: SpecParams): Position = specParams.position
  @inline def remaining(implicit specParams: SpecParams): Remaining = specParams.remaining
}

case class SpecResult(
  position: Position,
  remaining: Remaining,
  layout: Layout
)

case class Indent(value: Int) extends AnyVal
case class Width(value: Int) extends AnyVal
case class Layout(value: String) extends AnyVal
case class Position(value: Int) extends AnyVal
case class Remaining(value: Int) extends AnyVal
case class Horizontal(value: Boolean) extends AnyVal


case class Spec(fn: SpecParams => SpecResult) extends AnyVal {

  def apply(specParams: SpecParams): SpecResult = fn(specParams)
  def apply(indent: Indent,
            width: Width,
            horizontal: Horizontal,
            position: Position,
            remaining: Remaining): SpecResult = apply(SpecParams(indent, width, horizontal, position, remaining))
}

trait SpecDoc extends Doc[Spec]{
  import SpecParams._

  override def text(s: String): Spec =
    Spec(implicit params => SpecResult(
      position = Position(position.value + s.length),
      remaining = Remaining(remaining.value - s.length),
      layout = Layout(s)
    ))

  override def line: Spec =
    Spec{ implicit params =>
      val (r, l) = newLine(indent, width, horizontal, remaining)
      SpecResult(
        position = Position(position.value + 1),
        remaining = r,
        layout = l
      )
    }

  override def group(doc: Spec): Spec =
    Spec { implicit params =>
      val inner = doc(indent, width, Horizontal(true), position, remaining)
      if((inner.position.value - position.value) < remaining.value)
        inner
      else
        doc(indent, width, Horizontal(false), position, remaining)
    }

  override def concat(left: Spec, right: Spec): Spec = Spec{ implicit params =>
    val rLeft = left(indent, width, horizontal, position, remaining)
    val rRight = right(indent, width, horizontal, rLeft.position, rLeft.remaining)
    SpecResult(rRight.position, rRight.remaining, Layout(rLeft.layout.value ++ rRight.layout.value))
  }

  override def nest(indent: Indent, doc: Spec): Spec = Spec(implicit params =>
    doc(Indent(params.indent.value + indent.value), width, horizontal, position, remaining)
  )

  override def pretty(width: Width, doc: Spec): Layout =
    doc(Indent(0), width, Horizontal(false), Position(0), Remaining(width.value)).layout


  def newLine(implicit indent: Indent, width: Width, horizontal: Horizontal, remaining: Remaining): (Remaining, Layout) =
    if(horizontal.value)
      (Remaining(remaining.value - 1), Layout(" "))
    else
      (Remaining(width.value - indent.value), Layout(s"\n${List.fill(indent.value)(' ').mkString}"))
}

case class Norm[D](fn: D => (D, D))

class Normalized[D](implicit D: Doc[D]) extends Doc[Norm[D]]{
  import dsl._

  override def text(s: String): Norm[D] = Norm(d =>
    (D.text(s) <> d, D.nil)
  )

  override def line: Norm[D] = Norm(d =>
    (D.nil, D.line <> d)
  )

  override def group(doc: Norm[D]): Norm[D] = Norm(d => mapsnd(D.group)(doc.fn(d)))

  override def concat(dl: Norm[D], dr: Norm[D]): Norm[D] = Norm { tt =>
    val (tdr, sdr) = dr.fn(tt)
    val (tdl, sdl) = dl.fn(tdr)
    (tdl, sdl <> sdr)
  }

  override def nest(indent: Indent, doc: Norm[D]): Norm[D] = Norm(d => mapsnd[D, D](D.nest(indent, _))(doc.fn(d)))

  override def pretty(width: Width, doc: Norm[D]): Layout = {
    val (td, sd) = doc.fn(D.nil)
    D.pretty(width, td <> sd)
  }


  override def nil: Norm[D] = Norm(d => (d, D.nil))

  private def mapsnd[T, R](fn: T => R) : Tuple2[T, T] => Tuple2[T, R] = { case (x,y) => (x, fn(y))}
}

sealed trait Dequeue[T] {
  def enqueue(t: T): Dequeue[T] = append(t)
  def append(t: T): Dequeue[T] = Append(t, this)
  def prepend(t: T): Dequeue[T] = Prepend(t, this)

  private def emptyQueue: Nothing = throw new Exception("Empty dequeue")

  def dequeue: (T, Dequeue[T]) = {
    val trampoline = this match {
      case Empty() => emptyQueue
      case Append(elem, rest @ Empty()) => (elem, rest)
      case Prepend(elem, rest) => doPrepend(elem, rest)
      case Append(elem, rest) => doAppend(elem, rest)
    }
    trampoline.runT
  }

  private def doPrepend(elem: T, rest: Dequeue[T]): Trampoline[(T, Dequeue[T])] = Done((elem, remainder))
  @tailrec private def doAppend(elem: T, rest: Dequeue[T]): Trampoline[(T, Dequeue[T])] =
    More(() =>
      prefix match {
        case Empty() => emptyQueue
        case Prepend(elem2, rest2) => Done(elem2, )
        case Append(elem2, rest2) => doAppend(elem2, rest2.append(elem))
      }
    )



}
case class Empty[T]() extends Dequeue[T]
case class Prepend[T](elem: T, rest: Dequeue[T]) extends Dequeue[T]
case class Append[T](elem: T, rest: Dequeue[T]) extends Dequeue[T]