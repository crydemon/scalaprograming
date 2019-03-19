package s99


package arithmetic {

  class S99Int(val start: Int) {

    import S99Int._

    //需要参数，所以放class里
    def isCoprimeTo(m: Int): Boolean = gcd(m, start) == 1

    def isPrime: Boolean =
      (start > 1) && (primes.takeWhile(_ < Math.sqrt(start)) forall (start % _ != 0))
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter (_.isPrime))

    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)


    def main(args: Array[String]): Unit = {

      println(23.isCoprimeTo(24))
      println(gcd(24, 3))
      //      println(7.getClass)
      //      println("----------------")
      //      println(2.isPrime)
      //      println("----------------")
      //println(primes.take(50).toList)
    }
  }

}

