package s99


sealed abstract class Tree[+A] {
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def isSymmetric: Boolean
}

case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case _          => false
  }
  def isSymmetric: Boolean = left.isMirrorOf(right)
}

case object End extends Tree[Nothing] {
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true
  override def toString: String = "."
}

object Node {
  def apply[A](value: A): Node[A] = new Node(value, End, End)
}

object Tree {
  def cBalanced[A](nodes: Int, value: A): List[Tree[A]] = nodes match {
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

  def main(args: Array[String]): Unit = {
    println(Node('a', Node('b'), Node('c')).isSymmetric)
    println(cBalanced(13, "x").length)
  }
}

