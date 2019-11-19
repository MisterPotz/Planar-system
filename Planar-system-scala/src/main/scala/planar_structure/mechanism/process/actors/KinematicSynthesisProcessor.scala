package planar_structure.mechanism.process.actors

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem, Props}
import planar_interface.view.event_types.EventListener
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.process.report.KinematicSynthesisReport

import scala.concurrent.Future
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


//intented to check promises with given period
//in other words incapsulates the launch of actor system
trait KinematicSynthesisProcessorInterface{
  def startComputation() : Unit
  def performCallback() : Unit
  def setCompletionListener(eventListener : EventListener) : Unit
  //val completionPromise : Promise[KinematicSynthesisReport]
  var completionPromise : Future[KinematicSynthesisReport]
}

import akka.dispatch.ExecutionContexts
class KinematicSynthesisProcessor(val args : KinematicSynthesisArgs) extends KinematicSynthesisProcessorInterface {
  //создаем актор-систему
  val actorSystem = ActorSystem("Kinematic_Synthesis_System")
  //настраиваем роутер для рассылки по нодам
  val kinematicRouter: ActorRef = actorSystem.actorOf(Props[RouterKinematicActor])
  //интерфейс для исполнения колбека
  var eventListener : EventListener = _
  var completionPromise : Future[KinematicSynthesisReport] = _

  override def startComputation(): Unit = {
    implicit val timeout: Timeout = Timeout(15, TimeUnit.SECONDS)
    //футура для исполнения колбека при завершении обсчета
    completionPromise =     //асинхронный запрос
      ask(kinematicRouter, args).mapTo[KinematicSynthesisReport]
    //при выполнении футуры исполняем колбек
    completionPromise.onComplete[Any] {
      case Success(report) => performCallback()
      case Failure(exception) => println("Something gone wrong!")
    }(actorSystem.getDispatcher)
  }
  override def performCallback(): Unit = {
    import scala.concurrent.CanAwait
    //передаем в колбек полученный результат
    eventListener.callback(completionPromise.value.get.get)
  }
  //забрасываем вовнутрь интерфейс для колбека
  override def setCompletionListener(eventListener: EventListener): Unit = this.eventListener = eventListener
}
