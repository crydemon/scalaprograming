package highpp

class TagClass {

}

//It's important to note that user-defined value classes are limited, and in some circumstances,
case class Price(value: BigDecimal) extends AnyVal {
  def lowerThan(p:Price):Boolean = this.value < p.value
}

case class OrderId(value: Long) extends AnyVal

object TagClass {
  def printInfo(p: Price, oId: OrderId): Unit = {
  }

  def main(args: Array[String]): Unit = {
    val p1 = Price(BigDecimal(1.23))
    val p2 = Price(BigDecimal(34))
    p1.lowerThan(p2)
  }
}
