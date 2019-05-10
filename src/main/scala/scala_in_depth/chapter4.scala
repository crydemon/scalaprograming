package scala_in_depth

import collection.mutable.Stack
import org.scalatest._

trait Property{
  val name:String
  override def toString: String = "Property(" + name + ")"
}

trait Logger{
  def log(msg:String)= {

  }
}
trait NullLogger extends Logger{
  override def log(msg: String): Unit = {

  }
}
trait HasLogger{
  val logger: Logger = new Logger {}
}
trait HasNullLogger extends HasLogger{
  override val logger:Logger = new NullLogger {}
}

trait DataAccess extends HasLogger{
  def query[A](in:String) = {
    logger.log("QUERY" + in)
  }
}

object DataAccessSpec extends FlatSpec with Matchers {
  "A DataAccess Service" should "return queried data" in{
    val service = new DataAccess with HasNullLogger
    service.query("find")
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Property {
      override val name: String = "HI"
    }
    println(x)
  }
}
