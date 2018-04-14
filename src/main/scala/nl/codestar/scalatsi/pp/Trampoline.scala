package nl.codestar.scalatsi.pp

import scala.annotation.tailrec

sealed trait Trampoline[A] {
  @tailrec final def runT: A = this match {
    case More(fn) => fn().runT
    case Done(a) => a
  }
}

case class More[A](fn: () => Trampoline[A]) extends Trampoline[A]
object More {
  def apply[A](fn: => Trampoline[A]): Trampoline[A] = More(() => fn)
}

case class Done[A](a: A) extends Trampoline[A]
