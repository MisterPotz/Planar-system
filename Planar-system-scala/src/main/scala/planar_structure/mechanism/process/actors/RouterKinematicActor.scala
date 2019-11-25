package planar_structure.mechanism.process.actors
import java.util.concurrent.TimeUnit

import akka.pattern.{ask, pipe}
import akka.actor.{Actor, ActorRef, Props}
import akka.routing.{ActorRefRoutee, RoundRobinRoutingLogic, Routee, Router}
import akka.util.Timeout
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.mech2kh.static_methods.{CarrierDependentCalculations, MechanismTypeDependentCalculations, RatioEfficientCalculator}
import planar_structure.mechanism.process.argument.{AdditionalWheelParams, KinematicSynthesisArgs, MechanismCreatorArgs}
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
  sealed trait FilterPass
  case object InitialFilterPassed extends FilterPass
  case object AlignmentFilterPassed extends FilterPass
  case object SynthesizerPassed extends FilterPass
  case class Args(promise : Promise[Array[Mechanism]])
}

class RouteeManager(val targetCounts : Short){
  var counter : Int = 0
  def incrementCounter() : Unit = counter += 1
  def allReady() : Boolean = if (counter == targetCounts) true else false
}

class RouterKinematicActor extends Actor {
  //создаем актор-хранилище, куда будут отсылаться отфильтрованные механизмы
  val storageActor: ActorRef = context.actorOf(Props[KinematicSynthesisStorageActor])
  val lastRouteesManager = new RouteeManager(5)
  val alignmentRouteesManager = new RouteeManager(5)
  val synthesizerRouteeManager = new RouteeManager(5)
  val routerLastFilter : Router = {
    val routees = Vector.fill[Routee](lastRouteesManager.targetCounts){
        val actorRef = context.actorOf(Props(new KinematicRatioFilteredActor(storageActor)))
        context.watch(actorRef)
        ActorRefRoutee(actorRef)
    }
    //логика рассылки - равномерное распределение по дереву акторов
    Router(RoundRobinRoutingLogic(), routees)
  }
  val routerAlignmentFilter: Router = {
    val routees = Vector.fill[Routee](alignmentRouteesManager.targetCounts){
      val actorRef = context.actorOf(Props(new KinematicAlignmentActor(routerLastFilter)))
      context.watch(actorRef)
      ActorRefRoutee(actorRef)
    }
    //логика рассылки - равномерное распределение по дереву акторов
    Router(RoundRobinRoutingLogic(), routees)
  }
  val routerMechanismSynthesizer: Router = {
    val routees = Vector.fill[Routee](synthesizerRouteeManager.targetCounts){
      val actorRef = context.actorOf(Props(new MechanismSynthesizerActor(routerAlignmentFilter)))
      context.watch(actorRef)
      ActorRefRoutee(actorRef)
    }
    //логика рассылки - равномерное распределение по дереву акторов
    Router(RoundRobinRoutingLogic(), routees)
  }

  //the initial sender of upper level (KinematicSynthesisProcessor)
  var senderUpper : ActorRef = _
  var routeesLastReadyCounter : Int = 0
  var routeesAlignmentReadyCounter : Int = 0
  override def receive: Receive = {
    case a : KinematicSynthesisArgs =>
      this.senderUpper = sender()
      //делаем асинзронный запрос до момента, когда первичный фильтр будет пройден
      //тогда мы разошлем всем нодам месседж, который, когда они увидят, отошлют обратно
      //как только мы получим все колбеки от них, сделаем запрос хранилищу, как только она отошлет нам этот месседж обратно
      //то запросим данные у него. Короче говоря, ждем пока на каждом уровне выполнится его задача
      //после всего этого отправим результат sender()'у
      pipe(filterRatio(a))(context.dispatcher).to(self, sender = sender())

    case RouterKinematicActor.InitialFilterPassed => onInitialFilterPassed()
    case RouterKinematicActor.SynthesizerPassed => onMechanismSynthesizerPassed()
    case RouterKinematicActor.AlignmentFilterPassed=> onAlignmentFilterPassed()

    case MechanismSynthesizerActor.ThisActorIsReady => {
      synthesizerRouteeManager.incrementCounter()
      if (synthesizerRouteeManager.allReady()){
        self ! RouterKinematicActor.SynthesizerPassed
      }
    }
    case KinematicAlignmentActor.ThisActorIsReady =>{
      alignmentRouteesManager.incrementCounter()      //если счетчик готовности оказался равен числу наших узлов
      if (alignmentRouteesManager.allReady()){
        self ! RouterKinematicActor.AlignmentFilterPassed
      }
    }
    case KinematicRatioFilteredActor.ThisActorIsReady => {
      lastRouteesManager.incrementCounter()
      if (lastRouteesManager.allReady()){
        askStorage()
      }
    }
    case _ => println("Wrong input to router actor")
  }
  def askStorage() : Unit = {
    storageActor.ask(KinematicSynthesisStorageActor.Request)(Timeout(100, TimeUnit.MILLISECONDS))
      .mapTo[KinematicSynthesisStorageActor.Response]
      .onComplete((thing : Try[KinematicSynthesisStorageActor.Response]) => {
        thing match {
          case Success(value) => {
            senderUpper ! KinematicSynthesisReport(value.mechanisms)
          }
          case Failure(exception) => println("Some real bad shit happened")
        }
      })(context.dispatcher)
  }
  def onInitialFilterPassed() : Unit = {
    println("Initial filter worked")
    routerMechanismSynthesizer.routees.foreach(
      routee => routee.send(MechanismSynthesizerActor.UpperStageFinished, self))
  }
  def onMechanismSynthesizerPassed() : Unit = {
    println("Mechanisms synthesized worked")
    routerAlignmentFilter.routees.foreach{
      routee => routee.send(KinematicAlignmentActor.UpperStageFinished, self)
    }
  }
  def onAlignmentFilterPassed() : Unit = {
    println("AlignmentFilter worked")
    routerLastFilter.routees.foreach{
      routee => routee.send(KinematicRatioFilteredActor.UpperStageFinished, self)
    }
  }
  //при расчете передаточного отношения учитывать тип механизма
  /*protected def filterRatio3(args : KinematicSynthesisArgs,arrays : Array[ParVector[Short]]) : Unit ={
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
    }))}*/
  //case class WheelInfo(z: Short,ca : Float, ha: Float, x: Float, m : Float, alpha: Float, beta: Float)

  //при расчете передаточного отношения учитывать тип механизма
  protected def filterRatio3M(args : KinematicSynthesisArgs,arrays : Array[ParVector[Short]]) : Unit ={
    val upperBound = AccuracyCalculator.upperBound(args.eps_u1h, args.u1h)
    val lowerBound = AccuracyCalculator.lowerBound(args.eps_u1h, args.u1h)
    val first_stage_ratio = arrays(0).flatMap(z1 => arrays(1).map(z2 => {
      (z1, z2, RatioEfficientCalculator.calculateMiniRatio(z1,z2))
    }))
    val second_stage_ratio = arrays(1).flatMap(z2 => arrays(2).map(z3 => {
      (z2, z3, RatioEfficientCalculator.calculateMiniRatio(z2,z3))
    }))
    first_stage_ratio.foreach(u1 => second_stage_ratio.foreach(u2 => {
      val ratio = RatioEfficientCalculator.calculateGearRatio(u1._3, u2._3, args.carrierPosition,
        args.mechanismType)
      //если полученное отношение достаточно хорошо, отправляем все это синтезатору механизмов
      if (checkRatio(lowerBound, upperBound, ratio)) {
        if (!debug_Flag){
          println("ratio has passed")
          println(s"ratio: ${ratio}")
          debug_Flag = true
        }
        val wheelInfoList : List[WheelInfo] = List(
          WheelInfo(u1._1, args.additional(0).ca,args.additional(0).ha,args.additional(0).x,
            args.additional(0).m,args.additional(0).alpha,args.additional(0).beta),
          WheelInfo(u1._2, args.additional(1).ca,args.additional(1).ha,args.additional(1).x,
            args.additional(1).m,args.additional(1).alpha,args.additional(1).beta),
          WheelInfo(u2._2, args.additional(2).ca,args.additional(2).ha,args.additional(2).x,
            args.additional(2).m,args.additional(2).alpha,args.additional(2).beta)
        )
        routerMechanismSynthesizer.route(
          MechanismCreatorArgs(args.carrierPosition, args.mechanismType, wheelInfoList, args.k), self)
      }
    }))
  }
  var debug_Flag : Boolean = false

  protected def checkRatio(lowerBound: Float, upperBound : Float, found: Float) : Boolean = {
    //println(s"ratio: ${found}")
    if (found < 0){
      if (lowerBound > found && upperBound < found){
        true
      } else false
    } else{
      if (lowerBound < found && upperBound > found) true else false
    }
  }
  //при расчете передаточного отношения учитывать тип механизма
  protected def filterRatio4M(args : KinematicSynthesisArgs,arrays : Array[ParVector[Short]]) : Unit ={
    val upperBound = AccuracyCalculator.upperBound(args.eps_u1h, args.u1h)
    val lowerBound = AccuracyCalculator.lowerBound(args.eps_u1h, args.u1h)
    println(s"Lower bound: ${lowerBound}, upperBound : ${upperBound}")
    val first_stage_ratio = arrays(0).flatMap(z1 => arrays(1).map(z2 => {
      (z1, z2, RatioEfficientCalculator.calculateMiniRatio(z1,z2))
    }))
    val second_stage_ratio = arrays(2).flatMap(z3 => arrays(3).map(z4 => {
      (z3, z4, RatioEfficientCalculator.calculateMiniRatio(z3,z4))
    }))
    first_stage_ratio.foreach(u1 => second_stage_ratio.foreach(u2 => {
      val ratio = RatioEfficientCalculator.calculateGearRatio(u1._3, u2._3, args.carrierPosition,
        args.mechanismType)
      //если полученное отношение достаточно хорошо, отправляем все это синтезатору механизмов
      if (checkRatio(lowerBound, upperBound, ratio)) {
        if (!debug_Flag){
          println("ratio has passed")
          println(s"ratio: ${ratio}")
          debug_Flag = true
        }
        val wheelInfoList : List[WheelInfo] = List(
          WheelInfo(u1._1, args.additional(0).ca,args.additional(0).ha,args.additional(0).x,
            args.additional(0).m,args.additional(0).alpha,args.additional(0).beta),
          WheelInfo(u1._2, args.additional(1).ca,args.additional(1).ha,args.additional(1).x,
            args.additional(1).m,args.additional(1).alpha,args.additional(1).beta),
          WheelInfo(u2._1, args.additional(2).ca,args.additional(2).ha,args.additional(2).x,
            args.additional(2).m,args.additional(2).alpha,args.additional(2).beta),
          WheelInfo(u2._2, args.additional(3).ca,args.additional(3).ha,args.additional(3).x,
            args.additional(3).m,args.additional(3).alpha,args.additional(3).beta)
        )
        routerMechanismSynthesizer.route(
          MechanismCreatorArgs(args.carrierPosition, args.mechanismType, wheelInfoList, args.k), self)
      }
    }))
  }

  /*protected def filterRatio4(args : KinematicSynthesisArgs,arrays : Array[ParVector[Short]]) :Unit ={
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
  }*/

  def filterRatio(args : KinematicSynthesisArgs) : Future[RouterKinematicActor.FilterPass] = {
    val arrays = MechanismTypeDependentCalculations.createArraysAll(args.z_min_max, args.mechanismType)
    //теперь нам надо одновременно посортировать механизмы с учетом соосности и передаточного отношения
    //но этот перебор зависит от типа механизма, так что...
    args.mechanismType.wheelsAmount match {
      case 3 => filterRatio3M(args, arrays)//версия для трех колес
      case 4 => filterRatio4M(args, arrays)//версия для 4х колес
    }
    Future.successful(RouterKinematicActor.InitialFilterPassed)
  }
}
