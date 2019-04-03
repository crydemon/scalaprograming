package s99

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())

  // override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  override def toString = value.toString + children.map(_.toString + "^").mkString("")

  def nodeCount(): Int = children.foldLeft(1)(_ + _.nodeCount())
}

object MTree {
  implicit def string2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
  }

  def apply[T](value: T) = new MTree(value, List())

  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  def main(args: Array[String]): Unit = {
    println("afg^^c^bd^e^^".nodeCount())
    println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)
    //    println(MTree('a', List(MTree('f'))).nodeCount)
    //    println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)
  }
}