import akka.actor.Actor
import akka.util.Timeout
import akka.pattern.{ask, pipe}

import scala.concurrent.{Await, Future}
import akka.actor._

import scala.concurrent.duration._

trait ProbableInterface{
  def showMe(s : String) = println(s)
}

class SimpleInterfaceConcrete extends ProbableInterface

object AkkaActorMain extends App{
  val system = ActorSystem.create("Good_actor_system")
  val actor_test = system.actorOf(Props[TestActor])
  implicit val timeout = Timeout(5.seconds)
  println("asked first aktor")
  //val requested = actor_test.ask(Request)(timeout).andThen()
  //Await.result(requested,4.seconds)
  println("got the result in main thread")
}

final case class Result(x: Int, s: String, d: Double)
case object Request

class FiniteActor extends Actor{
  override def receive: Receive = {
    case Request =>
      println("sended back")
      //Thread.sleep(4000)
      sender() ! Result(5, "Catch", 2.3)
    case _ => println("what")
  }
}
import scala.concurrent.ExecutionContext.Implicits.global

class TestActor extends Actor{
    implicit val timeout = Timeout(3.seconds)
  val other_actor = context.actorOf(Props(new FiniteActor))
  override def receive: Receive = {
    case Request =>
      println("asked second aktor")
      val future = ask(other_actor, Request)(timeout).mapTo[Result]
      future.pipeTo(self)(sender = sender())
      //future.pipeTo(self)
      //sending back the results
    case Result(a,b,c) => println("test aktor got feedback from the last")
      //sender() ! Result(a,b,c)

  }
}


