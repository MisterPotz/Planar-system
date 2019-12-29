package planar_structure.mechanism.process.actors

import akka.actor.Actor
import akka.routing.Router
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.process.actors.KinematicRatioFilteredActor.CommonParams
import planar_structure.mechanism.process.argument.AdditionalWheelParams
object KinematicAlignmentActor{
  case class RatioFiltered4(w1: AdditionalWheelParams, w2: AdditionalWheelParams ,
                            w3: AdditionalWheelParams,w4: AdditionalWheelParams, commonParams: CommonParams)
  case class RatioFiltered3(w1: AdditionalWheelParams, w2: AdditionalWheelParams ,
                            w3: AdditionalWheelParams, commonParams: CommonParams)
  case object UpperStageFinished
  case object ThisActorIsReady
}


class KinematicAlignmentActor(val nextLayerRouter : Router) extends Actor{
  override def receive: Receive = {
    case KinematicAlignmentActor.UpperStageFinished => sender() ! KinematicAlignmentActor.ThisActorIsReady
    case a: Mechanism => checkAlignment(a)
    case KinematicAlignmentActor.RatioFiltered4(w1, w2, w3, w4, com) =>() //filter4(w1,w2,w3,w4,com)
    case KinematicAlignmentActor.RatioFiltered3(w1,w2,w3,com) =>() //filter3(w1,w2,w3,com)
  }
  var debug_Flag : Boolean = false
  def checkAlignment(mechanism: Mechanism) : Unit ={
    //если механизм пройдет пройдет проверку на соосность, отправляем следующему слою
    /*if (mechanism.methods.alignmentCondition){
      if (!debug_Flag){
        println("alignment has passed")
        debug_Flag = true
      }*/
      nextLayerRouter.route(mechanism, self)
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
  def filter3(w1: AdditionalWheelParams, w2: AdditionalWheelParams ,
              w3: AdditionalWheelParams, commonParams: CommonParams) : Unit = {
    //здесь проходим остальные условия синтеза и отсылаем в сторадж
    val mechanism : Mechanism = Mechanism2KH.apply(commonParams.mechanismType, commonParams.carrierPosition,List(w1.z, w2.z, w3.z),commonParams.k)
    //если механизм удовлетворил всем условиям - записываем его в хранилище
    if (mechanism.methods.assemblyCondition && mechanism.methods.neighborhoodCondition
      && mechanism.methods.noPruningOnAll && mechanism.methods.interferenceAll) {
      storageActor ! KinematicSynthesisStorageActor.Write(mechanism)
    }
  }*/
}
