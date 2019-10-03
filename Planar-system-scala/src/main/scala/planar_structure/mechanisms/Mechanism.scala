package planar_structure.mechanisms

import planar_structure.core_structure.{BaseGearWheel, BaseLink, BaseMechanism, Carrier, ChainLink, ChainLinkInterface, ExternalConnection, ExternalGearWheel, GearConnection, Input, InternalGearWheel, Output, Satellite}
import planar_structure.help_traits.{BeautifulDebugOutput, StorageHashDConnection, Updateable}
//TODO хранить всю карту в объекте-одиночке - это, конечно, круто, но надо сделать именно класс таких объектов хранителей
//TODO объявить четыре главных типа механизмов и заложить туда типовую структуру
//basic type of mechanism, additional mechanism can be addded to existing one
class Mechanism(val input_layer : ChainLink = new ChainLink(new StorageHashDConnection {})) extends BaseMechanism with Updateable with ChainLinkInterface[Mechanism] with BeautifulDebugOutput {
  //initial sequence of mechanism elements
  //получить итый механизм
  override def getLink(i: Int): BaseLink = {
    input_layer.getLink(i)
  }
  //получить итое соединение
  //TODO реализовано, но нужно потестить ещё
  override def getConnection(i: Int): GearConnection[BaseGearWheel, BaseGearWheel] = {
    input_layer.getConnection(i)
  }

  override def add[A <: Mechanism](a: A): Mechanism.this.type = ??? //TODO connect input and output, отложим до поры до времени эту фичу

  override def addAll[A <: Mechanism](all: A*): Mechanism.this.type = ??? //TODO massive connection between outputs and inputs, также отложим эту фичу
  override def installConnections(): Mechanism.this.type = {input_layer.installConnections(); this} //public full-connection update
  override  def installNewConnections(): Mechanism.this.type = ??? //emptying and creating new connections, without reinitializing
  override def getBranchSize: Int = input_layer.getBranchSize //full mechanism size
  override def copy: Mechanism.this.type = {new Mechanism(input_layer.copy)} //full copy of the whole mechanism
  override def toString: String = "Mechanism: " + print(input_layer.toString)

  override def toStringShort: String = "Mechanism:\n" + print(input_layer.toStringShort)

  override def getConnectionsAmount: Int = input_layer.getConnectionsAmount

  override def getConnectionStorage: StorageHashDConnection = input_layer.getConnectionStorage
}

sealed trait MechanismTypes extends Mechanism
/*
class SingleRowSimple extends MechanismTypes{
    //реализовать закладку структуры в конструкторе
}*/

trait TOneRowClassic extends MechanismTypes
trait TTwoRowEI extends MechanismTypes //external - internal two-row structure
trait TTwoRowIE extends MechanismTypes //internal - external two-row structure
trait TTwoRowEE extends MechanismTypes //external - external two-row structure

class OneRowClassic extends TOneRowClassic{
  override val input_layer: ChainLink = {
    val storageHashDConnection = new StorageHashDConnection {}
    new ChainLink(storageHashDConnection,new Input,
      new ExternalGearWheel(),
      //TODO addCarrierChainLink
      new Satellite(storageHashDConnection).addChainLink(0, new ExternalGearWheel(),new InternalGearWheel()),
      new Carrier, new Output
    )
  }

}