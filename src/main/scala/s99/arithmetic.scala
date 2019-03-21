package s99


package arithmetic {

  class S99Int(val start: Int) {

    import S99Int._

    //需要参数，所以放class里
    def isCoprimeTo(m: Int): Boolean = gcd(m, start) == 1

    def totient: Int = (1 to start) filter (start.isCoprimeTo(_)) length


    def primeFactors: List[Int] = {
      def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
        if (n.isPrime) List(n)
        else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
        else primeFactorsR(n, ps.tail)

      primeFactorsR(start, primes)
    }

    def primeFactorMultiplicity: List[(Int, Int)] = {
      def factorCount(n: Int, p: Int): (Int, Int) =
        if (n % p != 0) (0, n)
        else factorCount(n / p, p) match {
          case (c, d) => (c + 1, d)
        }

      def factorR(n: Int, ps: Stream[Int]): List[(Int, Int)] =
        if (n == 1) Nil
        else if (n.isPrime) List((n, 1))
        else {
          val nps = ps.dropWhile(n % _ != 0)
          val (count, dividend) = factorCount(n, nps.head)
          (nps.head, count) :: factorR(dividend, nps.tail)
        }

      def factorR1(n: Int, ps: Stream[Int]): List[(Int, Int)] = {

        def go(n: Int, nps: Stream[Int], result: List[(Int, Int)]): List[(Int, Int)] = {
          if (n == 1) Nil
          else if (n.isPrime) (n, 1) :: result
          else {
            val (count, dividend) = factorCount(n, nps.head)
            go(dividend, nps.tail, (nps.head, count) :: result)
          }
        }

        val nps = ps.dropWhile(n % _ != 0)
        go(start, nps, Nil)
      }

      factorR1(start, primes)
    }

    def totient1: Int = start.primeFactorMultiplicity.foldLeft(1)((r, f) =>
      f match {
        case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt
      })

    def listPrimesinRange(r: Range): List[Int] =
      primes dropWhile (_ < r.head) takeWhile (_ <= r.last) toList

    def goldbach: (Int, Int) = {
      def go(ps: Stream[Int]): (Int, Int) = {
        val (x1, x2) = (ps.head, start - ps.head)
        if (x2.isPrime) (x1, x2)
        else go(ps.tail)
      }

      go(primes takeWhile (_ <= start / 2))
    }

    def isPrime: Boolean = {
      (start > 1) && (primes.takeWhile(_ <= Math.sqrt(start)) forall (start % _ != 0))
    }

  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter (_.isPrime))

    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

    def printGoldbachList(l: Int, r: Int): Unit = {
      (l to r) filter (_ % 2 == 0) map (_.goldbach) foreach (println)
    }

    def printGoldbachListLimited(r: Range, limit: Int): Unit = {
      (math.max(r.head, limit) to r.last) filter (_ % 2 == 0) map (_.goldbach) filter (x => x._2 > limit && x._1 > limit) foreach (x => println(s"${x._1 + x._2}=${x._1}+${x._2}"))
    }


    def main(args: Array[String]): Unit = {
      printGoldbachListLimited(1 to 2000, 50)
      //printGoldbachList(9, 20)
      // println(28.goldbach)
      //      println(10.totient)
      //      println(23.isCoprimeTo(24))
      //      println(gcd(24, 3))
      //      println(7.getClass)
      //      println("----------------")
      //      println(2.isPrime)
      //      println("----------------")
      //println(primes.take(50).toList)
    }
  }

}

object P38 extends App {

  import arithmetic.S99Int._

  def time[A](label: String)(block: => A): A = {
    val now = System.currentTimeMillis()
    val ret = block
    println(label + ": " + (System.currentTimeMillis() - now) + " ms.")
    ret
  }

  def test(n: Int) {
    time("Preload primes") {
      primes takeWhile {
        _ <= Math.sqrt(n)
      } force
    }
    time("P34 (" + n + ")") {
      n.totient
    }
    time("P37 (" + n + ")") {
      n.totient1
    }
  }

  test(1000090)
}

