package domain

case class Foo(
  a: Int = 5,
  b: Int
)

case class FooOpt(
  a: Option[Int]
)
