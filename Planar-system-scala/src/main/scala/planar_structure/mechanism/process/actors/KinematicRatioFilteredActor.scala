package planar_structure.mechanism.process.actors

import akka.actor.{Actor, ActorRef}
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.process.actors.KinematicRatioFilteredActor.{CommonParams, ThisActorIsReady, UpperStageFInished, WheelParams}
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}
object KinematicRatioFilteredActor{
  case class WheelParams(z : Short)
  case class CommonParams(k : Byte, u1h : Float/*, eps_u1h : Int*/,
                          mechanismType: MechanismType, carrierPosition: CarrierPosition)
  case class RatioFiltered4(w1: WheelParams, w2: WheelParams ,
                            w3: WheelParams,w4: WheelParams, commonParams: CommonParams)
  case class RatioFiltered3(w1: WheelParams, w2: WheelParams ,
                            w3: WheelParams, commonParams: CommonParams)
  case object UpperStageFInished
  case object ThisActorIsReady
}

class KinematicRatioFilteredActor(val storageActor: ActorRef) extends Actor{
  override def receive: Receive = {
    case KinematicRatioFilteredActor.RatioFiltered4(w1, w2, w3, w4, com) => filter4(w1,w2,w3,w4,com)
    case KinematicRatioFilteredActor.RatioFiltered3(w1,w2,w3,com) => filter3(w1,w2,w3,com)
      //если нам пришло это сообщение - то значит других сообщений у нас не осталось и мы пусты
    case UpperStageFInished => sender() ! ThisActorIsReady
  }
  def filter4(w1: WheelParams, w2: WheelParams ,
              w3: WheelParams,w4: WheelParams, commonParams: CommonParams): Unit ={
    //здесь проходим остальные условия синтеза и отсылаем в сторадж
    val mechanism : Mechanism = Mechanism2KH.apply(commonParams.mechanismType,
      commonParams.carrierPosition,List(w1.z, w2.z, w3.z, w4.z),commonParams.k)
    //если механизм удовлетворил всем условиям - записываем его в хранилище
    if (mechanism.methods.assemblyCondition && mechanism.methods.neighborhoodCondition
      && mechanism.methods.noPruningOnAll && mechanism.methods.interferenceAll) {
      storageActor ! KinematicSynthesisStorageActor.Write(mechanism)
    }
  }
  def filter3(w1: WheelParams, w2: WheelParams ,
              w3: WheelParams, commonParams: CommonParams) : Unit = {
    //здесь проходим остальные условия синтеза и отсылаем в сторадж
    val mechanism : Mechanism = Mechanism2KH.apply(commonParams.mechanismType, commonParams.carrierPosition,List(w1.z, w2.z, w3.z),commonParams.k)
    //если механизм удовлетворил всем условиям - записываем его в хранилище
    if (mechanism.methods.assemblyCondition && mechanism.methods.neighborhoodCondition
        && mechanism.methods.noPruningOnAll && mechanism.methods.interferenceAll) {
      storageActor ! KinematicSynthesisStorageActor.Write(mechanism)
    }
  }
}
