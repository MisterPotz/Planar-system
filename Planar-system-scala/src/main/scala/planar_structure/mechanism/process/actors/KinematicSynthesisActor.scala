package planar_structure.mechanism.process.actors

/*
class KinematicSynthesisActor {

}

import java.util.{Calendar, ResourceBundle}
import java.util.concurrent.ForkJoinPool

import akka.actor.{Actor, ActorRef, PoisonPill, Props, SupervisorStrategy}
import akka.routing.{ActorRefRoutee, Broadcast, RoundRobinRoutingLogic, Router}

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParVector

case class Wheels3(z1: Short, z2: Short, z3: Short)
case class Wheels4(z1: Short, z2: Short, z3: Short, z4: Short)

class Wheel4Actor(val storageActor : ActorRef) extends Actor {
  //val storageActor: ActorRef = context.actorOf(Props(new StorageActor), "StorageActorOfProcess")
  override def receive: Receive = {
    case Wheels3(z1, z2, z3) => find4WheelAndSendIfGood(z1,z2,z3)
  }

he  override def postStop(): Unit = storageActor ! PoisonPill
  def find4WelAndSendIfGood(sh: Short, sh1: Short, sh2: Short) : Unit = {
    val z4 : Short = (sh - sh1 + sh2).toShort
    if (z4 > 0){
      //println(s"z1: $sh, z2: $sh1, z3: $sh2, z4: $z4")
      val ratio =  findRatio(sh, sh1, sh2,z4)
      if ((ratio < 1.3) && (ratio > 1.29)){
        storageActor ! Write(sh, sh1,sh2,z4)
      }
    }
  }
  def findRatio(z1 : Short,z2 : Short,z3 : Short , z4: Short): Float = {
    math.abs(1 - (z2 * z4) / (z1 * z3).toFloat)
  }
}


case class Write(a : Short, b: Short, c: Short, d : Short)
case object Stop
class StorageActor extends Actor{
  val storage = new mutable.Stack[(Short, Short, Short, Short)]()
  var finishTime: Calendar = null

  override def postStop(): Unit = {
    finishTime = Calendar.getInstance()
    println(s"Storage finished writing in ${finishTime.getTime}")
    println(storage.size)
    for (i <- Range(0, 10)){
      println(storage.pop())
    }
  }
  def receive = {
    case Write(a, b, c, d) => storage.addOne((a,b,c,d))
    case _ => ()
  }
}

case object StartCompute
case class CaseTest(a : String)
class ProcessActor extends Actor{
  val storageActor: ActorRef = context.actorOf(Props(new StorageActor), "StorageActorOfProcess")
  var router: Router = {
    val routees = Vector.fill(4) {
      val r = context.actorOf(Props(new Wheel4Actor(storageActor)))
      context.watch(r)
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }
  val wheel4 = context.actorOf(Props(new Wheel4Actor(storageActor)), "Wheels4")

  //val wheel4Actor = context.actorOf(Props(new Wheel4Actor), "StorageActorOfProcess")
  val z1_range = ParVector.range(17.toShort, 200.toShort)
  z1_range.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(5))
  val z2_range = ParVector.range(17.toShort, 200.toShort)
  z2_range.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(5))
  val z3_range = ParVector.range(17.toShort, 200.toShort)
  z3_range.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(5))
  //this one is for complete set that is good with alignment condition
  println("Initial preparation ready")
  override def receive: Receive = {
    case StartCompute => process
  }
  def process : Unit = {
    val now = Calendar.getInstance()
    println(s"${ResourceBundle.getBundle("some1").getString("some")}")
    println(s"Started ${now.getTime}")
    val now1 = Calendar.getInstance()
    //Thread.sleep(100)
    z1_range.foreach(z1 => z2_range.foreach(z2 => z3_range.foreach(z3 =>
      router.route(Wheels3(z1, z2, z3), self)

    )))
    //diff.foreach(nums => z3_range.foreach(z3 => wheel4Actor ! Wheels3(nums._1, nums._2, z3)))
    println(s"Ended in first actor : ${now1.getTime}")
    //Thread.sleep(1000)
    router.route(Broadcast(PoisonPill), self)
  }
}
*/
