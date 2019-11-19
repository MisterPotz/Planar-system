package planar_structure.mechanism.process.actors

import akka.actor.Actor
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.actors.KinematicSynthesisStorageActor.{Request, Response}

import scala.collection.mutable.ArrayBuffer

object KinematicSynthesisStorageActor{
  case class Write(mechanism : Mechanism)
  case object Request
  case class Response(mechanisms:  Array[Mechanism])
}

class KinematicSynthesisStorageActor extends Actor {
  //создаем массив куда будут записывать механизмы, успешно прошедшие все условия и фильтры
  val storageArray : ArrayBuffer[Mechanism] = ArrayBuffer.empty[Mechanism]
  override def receive: Receive = {
    case KinematicSynthesisStorageActor.Write(mechanism) => {
      //добавляем механизм в массив
      storageArray.addOne(mechanism)
    }
      //на запрос отправляем механизм
    case Request => sender() ! Response(storageArray.toArray)
    case _ => println("Bad request to storage actor")
  }
}
