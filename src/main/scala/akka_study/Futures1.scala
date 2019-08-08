package akka_study

// 1 - the imports
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Futures1 extends App {

  // used by 'time' method
  implicit val baseTime = System.currentTimeMillis

  // 2 - create a Future
  val f = Future {
    Thread.sleep(5000)
    1 + 1
  }.flatMap(_ => {
    Future {
      //Thread.sleep(1)
      println("en")
      1
    }
  })

  // 3 - this is blocking (blocking is bad)
  val result = Await.result(f, 100 second)
  println("yes")
  println(result)
  Thread.sleep(100000)

}