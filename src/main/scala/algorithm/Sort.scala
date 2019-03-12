package algorithm

object Sort {

  def quickSort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else {
      val arr = xs.toArray

      def swap(x: Int, y: Int) = {
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = tmp
      }

      def partition(n: Int, r: Int, pivot: Int): Int = {
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = n
        for (i <- n until r) {
          if (arr(i) < pivotVal) {
            if (i != j) {
              swap(i, j)
            }
            j += 1
          }
        }
        swap(j, r)
        j
      }

      def qs(n: Int, r: Int): Unit =
        if (n < r) {
          val pi = partition(n, r, n + (r - n) / 2)
          qs(n, pi - 1)
          qs(pi + 1, r)
        }

      qs(0, arr.length - 1)
      arr.toList
    }

  def main(args: Array[String]): Unit = {
    val xs = List.fill(1000)(scala.util.Random.nextInt(100000))
    println(xs)
    val sortXs = quickSort(xs);
    println(sortXs)

    def check(pre: Int, next: Int, xs: List[Int]): Boolean =
      if (pre > next) {
        println(pre + ", " + next)
        false
      } else if (xs.isEmpty) {
        true
      } else {
        check(next, xs.head, xs.tail)
      }

    println(check(sortXs.head, sortXs.head, sortXs))
  }

}
