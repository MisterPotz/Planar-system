package planar_structure.mechanism.process.actors
import java.util.concurrent.TimeUnit

import akka.pattern.{ask, pipe}
import akka.actor.{Actor, ActorRef, Props}
import akka.routing.{ActorRefRoutee, RoundRobinRoutingLogic, Routee, Router}
import akka.util.Timeout
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.static_methods.{CarrierDependentCalculations, MechanismTypeDependentCalculations}
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.process.report.KinematicSynthesisReport
import planar_structure.mechanism.types.Internal1

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
object AccuracyCalculator{
  def upperBound(error : Int, target : Float) : Float = {
    (1 + (0.01f * error)) * target
  }
  def lowerBound(error : Int, target : Float) : Float = {
    (1 - (0.01f * error)) * target
  }
}

object RouterKinematicActor{
  case class InitialFilterPassed()
  case class Args(promise : Promise[Array[Mechanism]])
}

class RouterKinematicActor extends Actor {
  //создаем актор-хранилище, куда будут отсылаться отфильтрованные механизмы
  val storageActor: ActorRef = context.actorOf(Props[KinematicSynthesisStorageActor])
  val routeesNumber : Int = 5
  val router : Router = {
    val routees = Vector.fill[Routee](routeesNumber){
        val actorRef = context.actorOf(Props(new KinematicRatioFilteredActor(storageActor)))
        context.watch(actorRef)
        ActorRefRoutee(actorRef)
    }
    //логика рассылки - равномерное распределение по дереву акторов
    Router(RoundRobinRoutingLogic(), routees)
  }
  //the initial sender of upper level (KinematicSynthesisProcessor)
  var senderUpper : ActorRef = _
  var routeesReadyCounter : Int = 0
  override def receive: Receive = {
    case a : KinematicSynthesisArgs =>
      this.senderUpper = sender()
      //делаем асинзронный запрос до момента, когда первичный фильтр будет пройден
      //тогда мы разошлем всем нодам месседж, который, когда они увидят, отошлют обратно
      //как только мы получим все колбеки от них, сделаем запрос хранилищу, как только она отошлет нам этот месседж обратно
      //то запросим данные у него. Короче говоря, ждем пока на каждом уровне выполнится его задача
      //после всего этого отправим результат sender()'у
      pipe(filterRatio(a))(context.dispatcher).to(self, sender = sender())
    case RouterKinematicActor.InitialFilterPassed() =>
      println("InitialFilter worked")
      router.routees.foreach(
      routee => routee.send(KinematicRatioFilteredActor.UpperStageFInished, self))
    case KinematicRatioFilteredActor.ThisActorIsReady =>{
      routeesReadyCounter += 1
      //если счетчик готовности оказался равен числу наших узлов
      if (routeesReadyCounter == routeesNumber){
        storageActor.ask(KinematicSynthesisStorageActor.Request)(Timeout(100, TimeUnit.MILLISECONDS))
          .mapTo[KinematicSynthesisStorageActor.Response]
          .onComplete((thing : Try[KinematicSynthesisStorageActor.Response]) => {
            thing match {
              case Success(value) => {
                //TODO sort mechanisms by size
                senderUpper ! KinematicSynthesisReport(value.mechanisms)
              }
              case Failure(exception) => println("Some real bad shit happened")
          }
          })(context.dispatcher)
      }
    }
    case Success(a : RouterKinematicActor.InitialFilterPassed) =>
      println("InitialFilter worked (success)")
      router.routees.foreach(
        routee => routee.send(KinematicRatioFilteredActor.UpperStageFInished, self))
    case _ => println("Wrong input to router actor")
  }
  //при расчете передаточного отношения учитывать тип механизма
  protected def filterRatio3(args : KinematicSynthesisArgs,arrays : Array[ParVector[Short]]) : Unit ={
    val upperBound = AccuracyCalculator.upperBound(args.eps_u1h, args.u1h)
    val lowerBound = AccuracyCalculator.lowerBound(args.eps_u1h, args.u1h)
    arrays(0).foreach(z1 => arrays(1).foreach(z2 => {
      val list = List(z1,z2)
      val z3 : Short = MechanismTypeDependentCalculations.z_last(list, args.mechanismType)
      if ((z3 >= args.z_min_max(2)._1) && (z3 <= args.z_min_max(2)._2)){
        val list1 = List(z1,z2,z3)
        val gearRatio: Float = CarrierDependentCalculations.calculateGearRatio(list1, args.carrierPosition, args.mechanismType)
        if ((gearRatio < upperBound) && (gearRatio > lowerBound)){
          val w1 = KinematicRatioFilteredActor.WheelParams(z1)
          val w2 = KinematicRatioFilteredActor.WheelParams(z2)
          val w3 = KinematicRatioFilteredActor.WheelParams(z3)
          val common = KinematicRatioFilteredActor.CommonParams(args.k, args.u1h, args.mechanismType, args.carrierPosition)
          router.route(KinematicRatioFilteredActor.RatioFiltered3(w1, w2,w3, common), self)
        }
      }
    }))
  }
  protected def filterRatio4(args : KinematicSynthesisArgs,arrays : Array[ParVector[Short]]) :Unit ={
    val upperBound = AccuracyCalculator.upperBound(args.eps_u1h, args.u1h)
    val lowerBound = AccuracyCalculator.lowerBound(args.eps_u1h, args.u1h)
    arrays(0).foreach(z1 => arrays(1).foreach(z2 => arrays(2).foreach(z3 => {
      val list = List(z1,z2,z3)
      val z4 : Short = MechanismTypeDependentCalculations.z_last(list, args.mechanismType)
      if ((z4 >= args.z_min_max(3)._1) && (z4 <= args.z_min_max(3)._2)){
        //println("прошел соосность")
        val list1 = List(z1,z2,z3,z4)
        val gearRatio: Float = CarrierDependentCalculations.calculateGearRatio(list1, args.carrierPosition, args.mechanismType)
        if ((gearRatio < upperBound) && (gearRatio > lowerBound)){
          val w1 = KinematicRatioFilteredActor.WheelParams(z1)
          val w2 = KinematicRatioFilteredActor.WheelParams(z2)
          val w3 = KinematicRatioFilteredActor.WheelParams(z3)
          val w4 = KinematicRatioFilteredActor.WheelParams(z4)
          val common = KinematicRatioFilteredActor.CommonParams(args.k, args.u1h, args.mechanismType, args.carrierPosition)
          router.route(KinematicRatioFilteredActor.RatioFiltered4(w1, w2,w3,w4, common), self)
        }
      }
    })))
  }

  def filterRatio(args : KinematicSynthesisArgs) : Future[RouterKinematicActor.InitialFilterPassed] = {
    val arrays = MechanismTypeDependentCalculations.createArrays(args.z_min_max, args.mechanismType)
    //теперь нам надо одновременно посортировать механизмы с учетом соосности и передаточного отношения
    //но этот перебор зависит от типа механизма, так что...
    args.mechanismType.wheelsAmount match {
      case 3 => filterRatio3(args, arrays)//версия для трех колес
      case 4 => filterRatio4(args, arrays)//версия для 4х колес
    }
    Future.successful(RouterKinematicActor.InitialFilterPassed())
  }
}
