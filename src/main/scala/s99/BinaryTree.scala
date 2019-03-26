package s99


sealed abstract class Tree[+T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean

  def isSymmetric: Boolean

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]

  def nodeCount: Int
  def leafCount: Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case _ => false
  }

  def isSymmetric: Boolean = left.isMirrorOf(right)

  override def addValue[U >: T <% Ordered[U]](x: U) =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))

  def nodeCount: Int = left.nodeCount + right.nodeCount + 1

  def leafCount: Int = {
   if (left == End && right == End)
      1
    else left.leafCount + right.leafCount
  }
}

//视界，就像类型边界，要求存在一个能够将某类型转换为指定类型的函数。你可以使用 <% 指定类型限制
case object End extends Tree[Nothing] {
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End

  def isSymmetric: Boolean = true

  override def toString: String = "."

  def addValue[U <% Ordered[U]](x: U) = Node(x)

  def nodeCount: Int = 0

  def leafCount: Int = 0
}

object Node {
  def apply[T](value: T): Node[T] = new Node(value, End, End)
}

object Tree {
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }

  def fromList[T <% Ordered[T]](l: List[T]): Tree[T] =
    l.foldLeft(End: Tree[T])((r, e) => r.addValue(e))

  def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] =
    cBalanced(nodes, value).filter(_.isSymmetric)

  def minHbalNodes(height: Int): Int = height match {
    case n if n < 1 => 0
    case 1 => 1
    case n => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
  }

  def maxHbalNodes(height: Int): Int = 2 * height - 1

  def minHbalHeight(nodes: Int): Int =
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1

  def maxHbalHeight(nodes: Int): Int =
    Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last

  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case 1          => List(Node(value))
    case _ => {
      val fullHeight = hbalTrees(height - 1, value)
      val short = hbalTrees(height - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
        fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }

  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
    (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList




  def main(args: Array[String]): Unit = {
    println(Node('x', Node('x'), End).leafCount)
    println( Tree.hbalTreesWithNodes(4, "x"))
//    val res = End.addValue(2)
//    println(res)
//    println(res.addValue(3))
//    println(res.addValue(3).addValue(0))
    //    println(Node('T', Node('b'), Node('c')).isSymmetric)
    //    println(cBalanced(13, "x").length)
  }
}

