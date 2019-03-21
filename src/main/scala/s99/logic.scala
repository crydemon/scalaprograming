package s99

class Logic(a: Boolean) {
  import Logic._
  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _            => false
  }
  def or(b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _         => false
  }
  def equ(b: Boolean): Boolean = (a and b) or (not(a) and not(b))
  def xor(b: Boolean): Boolean = not(a equ b)
  def nor(b: Boolean): Boolean = not(a or b)
  def nand(b: Boolean): Boolean = not(a and b)
  def impl(b: Boolean): Boolean = not(a) or b
}

object Logic {

  implicit def boolean2Logic(b: Boolean): Logic = new Logic(b)

  def not(a: Boolean) = a match {
    case true => false
    case false => true
  }

  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _ => false
  }

  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))

  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false)
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }

  def main(args: Array[String]): Unit = {
    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    table2((a: Boolean, b: Boolean) => a and (a or not(b)))
  }

}