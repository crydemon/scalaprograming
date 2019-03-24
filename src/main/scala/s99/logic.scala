package s99

class Logic(a: Boolean) {

  import Logic._

  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _ => false
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

  def gray(n: Int): List[String] = {
    def go(x: Int, result: List[String]): List[String] = {
      if (x <= 0) result
      else {
        val cur = result.map(s => ("0" + s)) ++ result.reverse.map(s => ("1" + s))
        go(x - 1, cur)
      }
    }

    go(n, List(""))
  }

  def main(args: Array[String]): Unit = {
    gray(4) foreach (println)
    //    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    //    table2((a: Boolean, b: Boolean) => a and (a or not(b)))
  }

}


object HuffmanCode {

  private abstract sealed class Tree[A] {
    val freq: Int

    def toCode: List[(A, String)] = toCodePrefixed("")

    def toCodePrefixed(prefix: String): List[(A, String)]
  }

  private final case class InternalNode[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    val freq: Int = left.freq + right.freq

    override def toCodePrefixed(prefix: String): List[(A, String)] =
      left.toCodePrefixed(prefix + "0") ::: right.toCodePrefixed(prefix + "1")
  }

  private final case class LeafNode[A](element: A, freq: Int) extends Tree[A] {
    override def toCodePrefixed(prefix: String): List[(A, String)] = List((element, prefix))
  }

  //维护两个有序队列， 一个是子节点，一个是生成的节点
  def huffman[A](xs: List[(A, Int)]): List[(A, String)] = {
    import collection.immutable.Queue
    def dequeueSmallest(q1: Queue[Tree[A]], q2: Queue[Tree[A]]): (Tree[A], Queue[Tree[A]], Queue[Tree[A]]) = {
      if (q2.isEmpty) (q1.front, q1.dequeue._2, q2)
      else if (q1.isEmpty || q2.front.freq < q1.front.freq) (q2.front, q1, q2.dequeue._2)
      else (q1.front, q1.dequeue._2, q2)
    }

    def huffmanR(q1: Queue[Tree[A]], q2: Queue[Tree[A]]): List[(A, String)] = {
      if (q1.length + q2.length == 1) (if (q1.isEmpty) q2.front else q1.front).toCode
      else {
        val (v1, q3, q4) = dequeueSmallest(q1, q2)
        val (v2, q5, q6) = dequeueSmallest(q3, q4)
        huffmanR(q5, q6.enqueue(InternalNode(v1, v2)))
      }
    }
    huffmanR(Queue.empty.enqueue(xs sortBy (r => r._2)) map (e => LeafNode(e._1, e._2)), Queue.empty)
  }

  def main(args: Array[String]): Unit = {
    huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) foreach(println)
  }

}