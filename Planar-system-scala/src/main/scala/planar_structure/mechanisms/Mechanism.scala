package planar_structure.mechanisms

import planar_structure.core_structure.{BaseGearWheel, BaseLink, BaseMechanism, Carrier, ChainLink, ChainLinkInterface, ExternalConnection, ExternalGearWheel, GearConnection, Input, InternalGearWheel, Output, Satellite}
import planar_structure.help_traits.{BeautifulDebugOutput, StorageHashDConnection, Updateable}

import scala.collection.mutable
//TODO объявить четыре главных типа механизмов и заложить туда типовую структуру - оформить в виде карты наверное будет лучше
//тогда можно будет пополнять эту карту из файла, доступ по стринг-названию
//basic type of mechanism, additional mechanism can be addded to existing one
class Mechanism(val input_layer : ChainLink = new ChainLink(new StorageHashDConnection {})) extends BaseMechanism with Updateable
  with ChainLinkInterface[Mechanism] with BeautifulDebugOutput {
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
  def debugPrint : String = {
    new String(s"Total chain branch size: ${input_layer.getBranchSize}") concat
    input_layer.toStringShort + "\n" concat
    new String(s"Installed connections: ${input_layer.getAllLinksAllowedForConnectionFull.length}") + "\n" concat
    {for (i <- 0 until input_layer.getConnectionsAmount)
      yield (input_layer.getConnection(i).toString + "\n")}.foldLeft("")(_+_)
  }
}

object MechanismClassStorage{
  def initMechanisms : Unit ={
    storage.addOne(("One Row", () => {
      val storage_ = new StorageHashDConnection {}
      val mech = new Mechanism(new ChainLink(storage_,new Input,
        new ExternalGearWheel(z = 40),
        new Satellite(storage_).addChainLink(0, new ExternalGearWheel(),new InternalGearWheel(230)),
        new Carrier, new Output
      ))
      mech.installConnections()
      mech
    }))
  }
  //storage with access to mechanism creators by string
  val storage : mutable.HashMap[String, () => Mechanism] = new mutable.HashMap[String,() => Mechanism]()

}

