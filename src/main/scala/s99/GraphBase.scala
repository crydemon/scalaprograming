package s99


//一个图应该有边，顶点，它们都是独立的，可以作为case class ,并且可以加入 新的节点
//应该给新类重写equals方法
//为什么不将加入边作为图最基本的行为呢
abstract class GraphBase[T, U] {


  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)

    override def toString = value match {
      case () => n1.value + edgeSep + n2.value
      case v => n1.value + edgeSep + n2.value + labelSep + v
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

    override def equals(o: Any): Boolean = o match {
      case o: Node => {
        o.value == value
      }
      case _ => false
    }

    def degree: Int = edges.foldLeft(0)((acc, e) => if (edgeTarget(e, Node(value)).getOrElse(false) == false) acc else acc + 1)

    def nodesByDepth(seen: Set[Node]): List[Node] = {
      def go(neighbors: List[Node], s: Set[Node]): List[Node] = neighbors match {
        case Nil => Nil
        case h :: t if s(h) => go(t, s)
        case h :: t => {
          val subNodes = h.nodesByDepth(s)
          subNodes ::: go(t, s ++ subNodes)
        }
      }

      go(neigthbors, seen + this) ::: List(this)
    }

    def partners: List[Node] =
      edges.map(edgeTarget(_, this)).filterNot(_.isEmpty).map(_.get).distinct
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  def edgeTarget(e: Edge, n: Node): Option[Node]

  def nodesByDegree: List[Node] = {
    nodes.foldLeft(Set[Node]())((acc, n) => acc + n._2).map(n => (n, n.degree)).toList.sortBy(r => r._2).reverse.map(n => n._1)
  }

  def colorNodes: List[(Node, Int)] = {
    def addColor(uncolored: List[Node], colored: List[(Node, Int)], adjacentNodes: Set[Node], color: Int): List[(Node, Int)] = uncolored match {
      case h :: t => {
        val newAdjacent = adjacentNodes ++ h.neigthbors
        addColor(t.filterNot(newAdjacent.apply), (h, color) :: colored, newAdjacent, color)
      }
      case _ => colored
    }

    def go(uncolored: List[Node], colored: List[(Node, Int)], color: Int): List[(Node, Int)] = {
      if (uncolored == Nil) {
        colored
      } else {
        val newColored = addColor(uncolored, colored, Set(), color)
        go(uncolored.diff(newColored.map(r => r._1)), newColored, color + 1)
      }
    }

    go(this.nodesByDegree, Nil, 1)
  }

  override def equals(o: Any) = o match {
    case g: GraphBase[_, _] => (nodes.keys.toList.diff(g.nodes.keys.toList) == Nil &&
      edges.map(_.toTuple).diff(g.edges.map(_.toTuple)) == Nil)
    case _ => false
  }

  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  def findPaths(source: T, dest: T): List[List[T]] = {
    def findPathsR(curNode: Node, curPath: List[T]): List[List[T]] = {
      if (curNode.value == dest) List(curPath)
      else curNode.adj.map(edgeTarget(_, curNode).get).filter(n => !curPath.contains(n.value)).flatMap(n => findPathsR(n, n.value :: curPath))
    }

    findPathsR(nodes(source), List(source)).map(_.reverse)
  }

  def findCycles(source: T): List[List[T]] = {
    val n = nodes(source)
    n.adj.map(edgeTarget(_, n).get.value).flatMap(findPaths(_, source)).map(source :: _).filter(_.lengthCompare(3) > 0)
  }

  //将二分图的结点划分归入两个子集A和B，则每一条图上的边都将连接一个A中的结点与一个B中的结点。
  //换言之，每一个结点与其相邻点之间都应该是来自不同子集，因为它们由一条边连结。
  def isBipartiteInternal: Boolean = {
    def isBipartiteR(evenToCheck: List[Node], oddToCheck: List[Node], evenSeen: Set[Node], oddSeen: Set[Node]): Boolean =
      (evenToCheck, oddToCheck) match {
        case (Nil, Nil) => true
        case (e :: eTail, odd) =>
          e.partners.forall(!evenSeen(_)) && isBipartiteR(eTail, odd.union(e.partners.filterNot(oddSeen(_))), evenSeen + e, oddSeen ++ e.partners)
        case (Nil, o :: oTail) =>
          o.partners.forall(!oddSeen(_)) && isBipartiteR(o.partners.filterNot(oddSeen(_)), oTail, evenSeen ++ o.partners, oddSeen + o)
      }

    isBipartiteR(nodes.values.toList, Nil, Set(), Set())
  }

  def isBipartite: Boolean = {
    nodes.isEmpty || splitGraph.forall(_.isBipartiteInternal)
  }

  def splitGraph: List[GraphBase[T, U]] = {
    def nodes2Graph(nodes: List[Node]): GraphBase[T, U] = {
      val adjacentForm = nodes.map(n => (n.value, n.adj.map(e =>
        (edgeTarget(e, n).get.value, e.value))))
      this match {
        case _: Graph[_, _] => Graph.adjacentLabel(adjacentForm)
        case _: Digraph[_, _] => Digraph.adjacentLabel(adjacentForm)
      }
    }

    def findConnectedNodes(candidates: List[Node], soFar: List[Node]): List[Node] =
      candidates match {
        case Nil => soFar
        case n :: tail => {
          val newNodes = n.partners.diff(n :: soFar)
          findConnectedNodes(tail.union(newNodes), n :: soFar)
        }
      }

    def splitGraphR(unsplit: List[Node]): List[GraphBase[T, U]] = unsplit match {
      case Nil => Nil
      case n :: _ => {
        val connectedNodes = findConnectedNodes(List(n), Nil)
        nodes2Graph(connectedNodes) :: splitGraphR(unsplit.diff(connectedNodes))
      }
    }

    splitGraphR(nodes.values.toList)
  }


}

//无向图
class Graph[T, U] extends GraphBase[T, U] {
  val edgeSep: String = "-"

  // edgeConnectsToGraph is needed for P84, so it's not an internal function.
  def edgeConnectsToGraph[T, U](e: Edge, nodes: List[Node]): Boolean =
    !(nodes.contains(e.n1) == nodes.contains(e.n2)) // xor
  def spanningTrees = {
    def spanningTreesR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): List[Graph[T, U]] = {
      if (graphNodes == Nil) List(Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple)))
      else if (graphEdges == Nil) Nil
      else graphEdges.filter(edgeConnectsToGraph(_, graphNodes)) flatMap { ge =>
        spanningTreesR(graphEdges.filterNot(_ == ge),
          graphNodes.filter(edgeTarget(ge, _) == None),
          ge :: treeEdges)
      }
    }

    def removeDuplicate(dup: List[Graph[T, U]], result: List[Graph[T, U]]): List[Graph[T, U]] = dup match {
      case h :: t => removeDuplicate(t.filterNot(i => h.equals(i)), h :: result)
      case _ => result
    }

    removeDuplicate(spanningTreesR(edges, nodes.values.toList.tail, Nil), Nil)
  }

  def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T, U] = {
    def minimalSpanningTreeR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): Graph[T, U] =
      if (graphNodes == Nil) Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple))
      else {
        val nextEdge = graphEdges.filter(edgeConnectsToGraph(_, graphNodes)).reduceLeft((r, e) => if (r.value < e.value) r else e)
        minimalSpanningTreeR(graphEdges.filterNot(_ == nextEdge),
          graphNodes.filter(edgeTarget(nextEdge, _) == None),
          nextEdge :: treeEdges)
      }

    minimalSpanningTreeR(edges, nodes.values.toList.tail, Nil)
  }


  def isTree: Boolean = spanningTrees.lengthCompare(1) == 0

  def isConnected: Boolean = spanningTrees.lengthCompare(0) > 0

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
    println(Graph.fromString("[a-b, c]").splitGraph)
    //println(Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes)
    //println(Graph.fromString("[a-b, b-c, b-e, a-e, a-c, a-d]").nodesByDegree)
    //println(Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree)
    //println(Graph.fromString("[a-b, b-c, a-c]").spanningTrees)
    //println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm)
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
    println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q"))
    //println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)
  }
}
