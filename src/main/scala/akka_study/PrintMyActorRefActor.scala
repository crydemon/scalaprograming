package akka_study

import akka.actor.{ Actor, ActorSystem, Props }
import scala.io.StdIn

object PrintMyActorRefActor {
  def props: Props =
    Props(new PrintMyActorRefActor)
}

class PrintMyActorRefActor extends Actor {
  override def receive: Receive = {
    case "printit" =>
      val secondRef = context.actorOf(Props.empty, "second-actor")
      println(s"Second: $secondRef")
  }
}

object ActorHierarchyExperiments extends App {
  val system = ActorSystem("testSystem")

  val firstRef = system.actorOf(PrintMyActorRefActor.props, "first-actor")
  println(s"First: $firstRef")
  // We sent the message by using the parent’s reference
  //Sends a one-way asynchronous message. E.g. fire-and-forget semantics.
  firstRef ! "printit"

  println(">>> Press ENTER to exit <<<")
  try StdIn.readLine()
  finally system.terminate()
}



//#start-stop
object StartStopActor1 {
  def props: Props =
    Props(new StartStopActor1)
}

class StartStopActor1 extends Actor {
  override def preStart(): Unit = {
    println("first started")
    context.actorOf(StartStopActor2.props, "second")
  }
  override def postStop(): Unit = println("first stopped")

  override def receive: Receive = {
    case "stop" => context.stop(self)
  }
}

object StartStopActor2 {
  def props: Props =
    Props(new StartStopActor2)
}

class StartStopActor2 extends Actor {
  override def preStart(): Unit = println("second started")
  override def postStop(): Unit = println("second stopped")

  // Actor.emptyBehavior is a useful placeholder when we don't
  // want to handle any messages in the actor.
  override def receive: Receive = Actor.emptyBehavior
}

object ActorHierarchyExperiments1 extends App {
  val system = ActorSystem("testSystem")
  val first = system.actorOf(StartStopActor1.props, "first")
  first ! "stop"
  //This ordering is strict, all postStop() hooks of the children are called before the postStop() hook of the parent is called.
  //system.terminate()
}

object SupervisingActor {
  def props:Props =
    Props(new SupervisingActor)
}
class SupervisingActor extends Actor{
  val child = context.actorOf(SupervisedActor.props, "supervised-actor")
  override def receive: Receive = {
    case "failChild" => child ! "fail"
  }
}

object SupervisedActor {
  def props: Props =
    Props(new SupervisedActor)
}

class SupervisedActor extends Actor {
  override def preStart(): Unit = println("supervised actor started")
  override def postStop(): Unit = println("supervised actor stopped")

  override def receive: Receive = {
    case "fail" =>
      println("supervised actor fails now")
      throw new Exception("I failed!")
  }
}

object ActorHierarchyExperiments2 extends App {
  val system = ActorSystem("testSystem")
  val supervisingActor = system.actorOf(SupervisingActor.props, "supervising-actor")
  supervisingActor ! "failChild"
}