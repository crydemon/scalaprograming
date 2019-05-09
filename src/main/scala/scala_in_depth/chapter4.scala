package scala_in_depth

trait Property{
  val name:String
  override def toString: String = "Property(" + name + ")"
}
object Test {
  def main(args: Array[String]): Unit = {
    val x = new Property {
      override val name: String = "HI"
    }
    println(x)
  }
}
