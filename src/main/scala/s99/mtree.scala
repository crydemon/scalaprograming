package s99

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())

  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  def nodeCount(): Int = children.foldLeft(1)(_ + _.nodeCount())
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())

  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  def main(args: Array[String]): Unit = {
    println(MTree('a', List(MTree('f'))).nodeCount)
    println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)
  }
}