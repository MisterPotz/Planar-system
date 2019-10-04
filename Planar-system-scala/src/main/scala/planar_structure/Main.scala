package planar_structure

import planar_structure.analysis.StructureAnalyser
import planar_structure.core_structure.{BaseGearWheel, BaseLink, ChainLink, ExternalConnection, ExternalGearWheel, GearConnection, InternalGearWheel, RecognizableBaseLink, Satellite}
import planar_structure.help_traits.{BeautifulDebugOutput, StorageHashDConnection}
import planar_structure.mechanisms.{Mechanism, MechanismClassStorage}

import scala.collection.mutable.ListBuffer
class Tests extends BeautifulDebugOutput{
  override def toString: String = print("whoa\ncool")
}
class subTest extends Tests{
  override def toString: String = print(super.toString)
}

object Main extends App with RecognizableBaseLink {
  //TODO функция нахождения пригодных для соединений звеньев работает, но лучше потестить
  MechanismClassStorage.initMechanisms
  val onerow_mechanism = MechanismClassStorage.storage("One Row")()
  println(onerow_mechanism.debugPrint)
/*
  onerow_mechanism.input_layer.connections_storage.collection.apply((onerow_mechanism.getLink(1), onerow_mechanism.getLink(2)))
*/
  println(onerow_mechanism.input_layer.connections_storage.collection.keys)
  println(onerow_mechanism.getConnection(1))
//  println(StructureAnalyser.getRatioFunc(onerow_mechanism))
  /*val all_connections : ListBuffer[BaseLink] =  onerow_mechanism.input_layer.getAllLinksAllowedForConnectionFull.flatMap(a => onerow_mechanism.input_layer.connections_storage((a._1, a._2))._2)
  all_connections.foreach(a => println(a.toStringShort))*/
}