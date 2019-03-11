object chapter13 extends App {

  val f = (x: Int) => x + 1
  val g = List.fill(1000000)(f).foldLeft((x: Int) => x + 2)(_ compose _)
  val h = List.fill(10000)(f)
  //g(42)
  println(g(0))
}
