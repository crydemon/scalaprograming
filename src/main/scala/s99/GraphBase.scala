package s99

//一个图应该有边，顶点，它们都是独立的，可以作为case class ,并且可以加入 新的节点
//应该给新类重写equals方法
//为什么不将加入边作为图最基本的行为呢
abstract class GraphBase[T, U] {

  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)

    override def toString = value match {
      case () => n1.value + edgeSep + n2.value
      case v  => n1.value + edgeSep + n2.value + labelSep + v
    }
  }

  val edgeSep: String
  val labelSep: String = "/"

  override def toString = {
    val (edgeStrs, unlinkedNodes) =
      edges.foldLeft((Nil: List[String], nodes.values.toList))((r, e) => (e.toString :: r._1, r._2.filter((n) => n != e.n1 && n != e.n2)))
    "[" + (unlinkedNodes.map(_.value.toString) ::: edgeStrs).mkString(", ") + "]"
  }
  def toTermForm: (List[T], List[(T, T, U)]) =
    (nodes.keys.toList, edges.map((e) => (e.n1.value, e.n2.value, e.value)))
  def toAdjacentForm: List[(T, List[(T, U)])] =
    nodes.values.toList.map((n) => (n.value, n.adj.map((e) =>
      (edgeTarget(e, n).get.value, e.value))))
  //用邻接矩阵的方式存储
  case class Node(value: T) {
    var adj: List[Edge] = Nil

    def neigthbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_, _] => (nodes.keys.toList.diff(g.nodes.toList) == Nil &&
      edges.map(_.toTuple).diff(g.edges.map(_.toTuple)) == Nil)
    case _ => false
  }

  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }
}

//无向图
class Graph[T, U] extends GraphBase[T, U] {
  val edgeSep: String = "-"
  override def equals(o: Any) = o match {
    case g: Graph[_, _] => super.equals(g)
    case _ => false
  }

  override def edgeTarget(e: Edge, n: Node): Option[Node] = {
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None
  }

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }
}
//有向图
class Digraph[T, U] extends GraphBase[T, U] {
  val edgeSep: String = ">"
  override def equals(o: Any): Boolean = o match {
    case g: Digraph[_, _] => super.equals(g)
    case _ => false
  }

  override def edgeTarget(e: Edge, n: Node): Option[Node] = {
    if (e.n1 == n) Some(e.n2)
    else None
  }

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }
}

abstract class GraphObjBase {
  type GraphClass[T, U]

  val edgeSep: String
  val labelSep: String = "/"

  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))

  def term[T](nodes: List[T], edges: List[(T, T)]) =
    termLabel(nodes, addLabel(edges))

  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]): GraphClass[T, U]

  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))

  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))

  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]): GraphClass[T, U]

  def fromStringBase[U, V](s: String)(mkGraphOrDigraph: (List[String], List[V]) => GraphClass[String, U])(parseEdge: String => V): GraphClass[String, U] = {
    assert(s(0) == '[')
    assert(s(s.length - 1) == ']')
    val tokens = s.substring(1, s.length - 1).split(", *").toList
    val nodes = tokens.flatMap(_.replaceAll("/.*", "").split("[->]")).distinct
    val edges = tokens.filter(_.matches(".*[->].*")).map(parseEdge)
    mkGraphOrDigraph(nodes, edges)
  }

}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]
  val edgeSep: String = "-"
  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }


  def fromString(s: String): GraphClass[String, Unit] =
    fromStringBase(s)(Graph.term[String]) { t =>
      val split = t.split("[->]")
      (split(0), split(1))
    }


  def fromStringLabel(s: String): GraphClass[String, Int] =
    fromStringBase(s)(Graph.termLabel[String, Int]) { t =>
      val split = t.split("-")
      val split2 = split(1).split("/")
      (split(0), split2(0), split2(1).toInt)
    }


  override def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]): Graph[T, U] = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neigthbors.contains(g.nodes(n2))) {
        g.addEdge(n1, n2, l)
      }
    }
    g
  }

  def main(args: Array[String]): Unit = {
    println( Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm)
  }
}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]
  val edgeSep: String = ">"
  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }

  def fromString(s: String): GraphClass[String, Unit] =
    fromStringBase(s)(Digraph.term[String]) { t =>
      val split = t.split(">")
      (split(0), split(1))
    }

  def fromStringLabel(s: String): GraphClass[String, Int] =
    fromStringBase(s)(Digraph.termLabel[String, Int]) { t =>
      val split = t.split(">")
      val split2 = split(1).split("/")
      (split(0), split2(0), split2(1).toInt)
    }

  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }

  def main(args: Array[String]): Unit = {
    println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)
  }
}