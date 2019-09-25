package planar_structure.core_structure

import planar_structure.help_traits._

import scala.language.implicitConversions


trait LinkManipulator {
  def getLink(i: Int): BaseLink;
}
trait ConnectionManipulator{
  def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
}
trait ChainLinkInterface[T] extends Addable[T] with Updateable with LinkManipulator with ConnectionManipulator with
RecognizableBaseLink
{self =>
  override def getLink(i : Int) : BaseLink
  override def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
  //appending unique element to storages
  override def add[A <: T](a: A): self.type
  override def addAll[A <: T](all: A*): self.type
  //create or update connections
  def installConnections() : self.type
  //removes old ones if there are not enough connections and creates new ones
  protected def installNewConnections() : self.type
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  override def update(): self.type

}
//basic chain of connections and wheels
class ChainLink extends ChainLinkInterface[BaseLink] {
//initializing array of mechanisms
  lazy val gear_storage: Storage[BaseLink] = new Storage[BaseLink] {}
  lazy val connections_storage : Storage[GearConnection[BaseGearWheel, BaseGearWheel]] = new Storage[GearConnection[BaseGearWheel, BaseGearWheel]] {}
  //get link i
  override def getLink(i : Int) : BaseLink ={
    //if the index is bigger than the size of the storage - search for satellit
    gear_storage(i)
  }
  override def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel] = connections_storage(i)
  //TODO create mechanism from file (JSON)
  override def toString: String = "\nChain link, gears:" concat gear_storage.toString + "\n----------->gear connections:\n" concat connections_storage.toString
  //appending unique element to storages
  override def add[A <: BaseLink](a: A): ChainLink.this.type = {
    a match {
      case a : BaseGearWheel => gear_storage.addOne(a)
      case a : GearConnection[BaseGearWheel, BaseGearWheel] => connections_storage.addOne(a)
      case a : Satellite => gear_storage.addOne(a)
      case _ => println("Element was declined, not appended")
    }
    this
  }
  override def addAll[A <: BaseLink](all: A*): ChainLink.this.type = {
    for (a <- all) add(a)
    this
  }
  //create or update connections
  def installConnections() : ChainLink.this.type  = update()
  //removes old ones if there are not enough connections and creates new ones
  protected def installNewConnections() : ChainLink.this.type = {
    val indeces_to_pick = for (i <- 0 until gear_storage.length-1 ) yield (i, i+1)
    for (i <- 0 until connections_storage.length)
      connections_storage.remove(0)
    for ((i, k) <- indeces_to_pick){
      //TODO  {BaseGear -> Ext and Int. : done} implicit function to create (and check internally) a gear from first satellites gear
      val good : Boolean = (gear_storage(i), gear_storage(k)) match {
        case (i: BaseGearWheel, k: BaseGearWheel) => true;
        case (i : BaseGearWheel, k: Satellite) => true
        case _ => false}
      if (good) {
        connections_storage.addOne(GearConnection((gear_storage(i)),(gear_storage(k))))
      }
    }
    this
  }
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  override def update(): ChainLink.this.type = {
    val is_equipped = if (connections_storage.length < gear_storage.length - 1) false else true
    //delete old connections if there are less of them than it should
    if (!is_equipped) {
      installNewConnections()
    }
    //or just reinit them if there are as much connections as it should
    else {
      for ( i <- 0 until connections_storage.length) {
        connections_storage(i).init
        //basically this does nothing but in case of satellite - then it updates satellites connections
        //so a command from one point re-initializes connections all over the rest mechanism
        gear_storage(i).update()
      }
      gear_storage(gear_storage.length-1).update()
    }
    this
  }
}


