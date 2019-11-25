package planar_structure.mechanism.process.actors

import akka.actor.Actor
import akka.routing.Router
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.process.argument.MechanismCreatorArgs
object MechanismSynthesizerActor{
  case object UpperStageFinished
  case object ThisActorIsReady
}

//used for synthesizing mechanisms
class MechanismSynthesizerActor(val routerNextLayer: Router ) extends Actor{
  override def receive: Receive = {
    //если нам пришло это сообщение - то значит других сообщений у нас не осталось и мы пусты
    case MechanismSynthesizerActor.UpperStageFinished => sender() ! MechanismSynthesizerActor.ThisActorIsReady
    case a: MechanismCreatorArgs => createMechanismAndSend(a)
  }
  def createMechanismAndSend(args: MechanismCreatorArgs) : Unit = {
    val mechanism : Mechanism = Mechanism2KH.apply(args.mechanismType,args.carrierPosition,args.wheelParams,args.k)
    routerNextLayer.route(mechanism, self)
  }

}
