package planar_structure.mechanism.process.actors

import akka.actor.{Actor, ActorRef}
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.process.actors.KinematicRatioFilteredActor.{CommonParams, ThisActorIsReady}
import planar_structure.mechanism.process.argument.AdditionalWheelParams
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}
object KinematicRatioFilteredActor{
  case class CommonParams(k : Byte, u1h : Float/*, eps_u1h : Int*/,
                          mechanismType: MechanismType, carrierPosition: CarrierPosition)
  case class AlignmentClearMechanism(mech : Mechanism)
  case object UpperStageFinished
  case object ThisActorIsReady
}

class KinematicRatioFilteredActor(val storageActor: ActorRef) extends Actor{
  override def receive: Receive = {
      //если нам пришло это сообщение - то значит других сообщений у нас не осталось и мы пусты
    case KinematicRatioFilteredActor.UpperStageFinished => sender() ! ThisActorIsReady
    case a:  Mechanism => checkLeftConditions(a)
  }
  def checkLeftConditions(mechanism: Mechanism) : Unit = {
    /*if (mechanism.methods.assemblyCondition && mechanism.methods.neighborhoodCondition
      && mechanism.methods.noPruningOnAll && mechanism.methods.interferenceAll) {
      storageActor ! KinematicSynthesisStorageActor.Write(mechanism)
    }*/
  }
  /*def filter4(w1: AdditionalWheelParams, w2: AdditionalWheelParams ,
              w3: AdditionalWheelParams,w4: AdditionalWheelParams, commonParams: CommonParams): Unit = {
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
  }*/
}
