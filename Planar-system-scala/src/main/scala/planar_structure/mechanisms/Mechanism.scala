package planar_structure.mechanisms

import planar_structure.core_structure.{BaseGearWheel, BaseLink, BaseMechanism, ChainLink, ChainLinkInterface, ExternalConnection, ExternalGearWheel, GearConnection}
import planar_structure.help_traits.{BeautifulDebugOutput, Updateable}
//basic type of mechanism, additional mechanism can be addded to existing one
class Mechanism(val input_layer : ChainLink = new ChainLink()) extends BaseMechanism with Updateable with ChainLinkInterface[Mechanism] with BeautifulDebugOutput {
  //initial sequence of mechanism elements
  //получить итый механизм
  override def getLink(i: Int): BaseLink = {
    input_layer.getLink(i)
  }
  //получить итое соединение
  //TODO а как это реализовать? а нужно ли это? хм, вероятно, нужно
  override def getConnection(i: Int): GearConnection[BaseGearWheel, BaseGearWheel] = {
    input_layer.getConnection(i)
  }

  override def add[A <: Mechanism](a: A): Mechanism.this.type = ??? //TODO connect input and output

  override def addAll[A <: Mechanism](all: A*): Mechanism.this.type = ??? //TODO massive connection between outputs and inputs

  override def installConnections(): Mechanism.this.type = {input_layer.installConnections(); this} //public full-connection update

  override  def installNewConnections(): Mechanism.this.type = ??? //emptying and creating new connections, without reinitializing

  override def getBranchSize: Int = input_layer.getBranchSize //full mechanism size

  override def copy: Mechanism.this.type = {new Mechanism(input_layer.copy)} //full copy of the whole mechanism
  override def toString: String = "Mechanism: " + print(input_layer.toString)
}

sealed trait MechanismTypes extends Mechanism
/*
class SingleRowSimple extends MechanismTypes{
    //реализовать закладку структуры в конструкторе
}*/
