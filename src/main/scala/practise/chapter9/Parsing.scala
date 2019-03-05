package practise.chapter9

import scala.util.matching.Regex

object Parsing {

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

    def advanceBy(n: Int): Location = {
      copy(offset = offset + n)
    }
  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(location: Location, message: String) =
      copy(stack = (location, message) :: stack)

    def latestLoc: Option[Location] =
      latest map (_._1)

    def latest: Option[(Location, String)] =
      stack.lastOption

    def label(s: String): ParseError = {
      ParseError(latestLoc.map((_, s)).toList)
    }
  }

  trait Parsers[Parser[+ _]] {
    self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = p | p2

      def map[B](f: A => B): Parser[B] = self.map(p)(f)
    }

    def char(c: Char): Parser[Char] =
      string(c.toString) map { s: String => s.charAt(0) }

    def succeed[A](a: A): Parser[A]
    = string("") map (_ => a)

    def slice[A](p: Parser[A]): Parser[String]

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

    /** Exercise 1
      *
      * Using product, implement map2, then use this to implement many1 in terms of many
      */

    def map2_1[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2) map {
      case ((a: A, b: B)) => f(a, b)
    }

    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

    /** Exercise 2
      *
      * Try coming up with laws to specify the behaviour of product
      *
      * product(a, ()).map(fst) == a
      * product((), a).map(snd) == a
      */

    /** Exercise 3
      *
      * Define many in terms of or, map2 and succeed
      */
    def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p, many(p))(_ :: _), succeed(Nil))


    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for {a <- p; b <- p2} yield f(a, b)

    /** Exercise 4
      *
      * Using map2 and succeed, implement listOfN combinator
      */

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n < 1) succeed(Nil)
      else map2(p, listOfN(n - 1, p))(_ :: _)


    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    implicit def regex(r:Regex):Parser[String                                                                                                                    ]

    /** Exercise 8
      *
      * Implement map in terms of flatMap and other combinators
      */
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)


  }

}
