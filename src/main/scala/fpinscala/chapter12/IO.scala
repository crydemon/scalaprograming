package fpinscala.chapter12

sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](b: A) extends IO[A]

case class Suspend[A](resume: () => A) extends IO[A]

case class FlatMap[A, B](value: IO[A], function: A => IO[B]) extends IO[B]


