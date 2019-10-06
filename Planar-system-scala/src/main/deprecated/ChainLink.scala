package deprecated

import planar_structure.core_structure.connections.GearConnection
import planar_structure.core_structure.links
import planar_structure.help_traits._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.implicitConversions


trait LinkManipulator {
  def getLink(i: Int): BaseLink;
}
trait ConnectionManipulator{
  def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
}

trait ChainLinkInterface[T] extends Addable[T] with Updateable with LinkManipulator with ConnectionManipulator with
RecognizableBaseLink with BeautifulDebugOutput with Storable
{self =>
  override def getLink(i : Int) : BaseLink
  override def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
  //appending unique element to storages
  override def add[A <: T](a: A): ChainLinkInterface[T]
  override def addAll[A <: T](all: A*): ChainLinkInterface[T]
  def getConnectionsAmount : Int
  //create or update connections
  def installConnections() : ChainLinkInterface[T]
  //removes old ones if there are ???not enough connections and creates new ones
  protected def installNewConnections() : ChainLinkInterface[T]
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  override def update(): ChainLinkInterface[T]
  def getBranchSize : Int
  def copy : ChainLinkInterface[T]
  def toStringShort : String
  def getConnectionStorage : StorageHashDConnection
}
//basic chain of connections and wheels
class ChainLink(val connections_storage : StorageHashDConnection,baseLink: BaseLink*) extends ChainLinkInterface[BaseLink] {
  init
//initializing array of mechanisms
  def init : Unit = {
    baseLink.foreach((f:BaseLink )=> gear_storage.addOne(f))
  }
  override def getConnectionStorage: StorageHashDConnection = connections_storage
  lazy val gear_storage: Storage[BaseLink] = new Storage[BaseLink] {}
  //reference to common storage of connections
  //get link i
  protected def getLinkRec(i : Int, current : Int) : BaseLink = {
   if (gear_storage.collection(current).getBranchSize - 1 >= i){
     gear_storage.collection(current).getLink(i)
   }
    else
     getLinkRec(i-gear_storage.collection(current).getBranchSize, current+1)
  }
  override def getLink(i : Int) : BaseLink = {
    getLinkRec(i, 0)
  }
  def allowedForConnection(baseLink: BaseLink, baseLink2: BaseLink) : Boolean = {
    (baseLink, baseLink2) match {
      case (i: BaseGearWheel, k: BaseGearWheel) => true;
      case (i : BaseGearWheel, k: deprecated.Satellite) => true
      case _ => false}
  }
  def getAllLinksAllowedForConnectionFull : ListBuffer[(BaseLink, BaseLink)] = {
    val all_allowed_in_this: Array[(BaseLink, BaseLink)] = getAllLinksAllowedForConnection
    if (all_allowed_in_this.isEmpty)
      return new ListBuffer[(BaseLink, BaseLink)]
    all_allowed_in_this(all_allowed_in_this.length - 1)._2 match {
      // если последний элемент  - сателлит, то тыкаем его, извлекаем оттуда все что нужно и отдаем выше
      case a: deprecated.Satellite => new ListBuffer[(BaseLink, BaseLink)].appendAll(all_allowed_in_this).appendAll(a.getAllLinksAllowedForConnectionFull)
      //ListBuffer[(BaseLink,BaseLink)](all_allowed_in_this.toList).appendAll(a.getAllLinksAllowedForConnectionFull)
      //если что-то другое, возвращаем что нашли на этом уровне
      case _ => new ListBuffer[(BaseLink, BaseLink)].appendAll(all_allowed_in_this)
    }
  }
  //is used to scrape pairs of indexes of gear_storage objects for creation of connections
  def availableIndeces : Seq[(Int, Int)] = for (i <- 0 until gear_storage.length - 1) yield (i, i+1)
  def getAllLinksAllowedForConnection : Array[(BaseLink,BaseLink)] = {
    val indeces = availableIndeces
    val array = new ArrayBuffer[(BaseLink, BaseLink)]()
    for (i <- indeces){
      if (allowedForConnection(gear_storage(i._1), gear_storage(i._2))){
        array.addOne((gear_storage(i._1), gear_storage(i._2)))
      }
    }
    //creating available connections only in this chainlink
    array.toArray
  }
  override def getConnectionsAmount : Int ={
    connections_storage.length
  }
  //TODO create mechanism from file (JSON)
  override def toString: String = "\nChain link:" + print("\ngears:\n" +gear_storage.toString) //+ "\n" + "connections:\n" + connections_storage.toString)
  //appending unique element to storages
  def add[A <: BaseLink](a: A): ChainLink = {
    a match {
      case a : BaseGearWheel => gear_storage.addOne(a)
      case a : deprecated.Satellite => gear_storage.addOne(a)
      case _ => println("Element was declined, not appended")
    }
    this
  }
  def addAll[A <: BaseLink](all: A*): ChainLink = {
    for (a <- all) add(a)
    this
  }
  def fullConnectionsClean() : ChainLink = {
    connections_storage.collection.clear()
    this
  }
  //create or update connections
  def installConnections() : ChainLink  = update()
  //removes old ones if there are not enough connections and creates new ones
  protected def installNewConnections() : ChainLink= {
    //just creating new array with certified baselinks to be sure that no bad things will happen
    val gears_to_connect = getAllLinksAllowedForConnectionFull
    for (i <- gears_to_connect) {
      connections_storage.addOne((i.asInstanceOf[(BaseGearWheel,BaseGearWheel)], GearConnection(i._1,i._2)))
    }
    this
  }
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  override def update(): ChainLink = {
    installNewConnections()
    this
  }

  override def getBranchSize: Int = {
    gear_storage.collection.foldLeft(0)((left : Int, right: BaseLink) => left + right.getBranchSize)
  }
  override def copy : ChainLink  ={
    val chainLink : ChainLink = new ChainLink(this.connections_storage)
    for (i <- 0 until gear_storage.length)
      chainLink.add(gear_storage(i).copy)
    chainLink
  }

  override def getConnection(i: Int): GearConnection[BaseGearWheel, BaseGearWheel] = {
    val units = getAllLinksAllowedForConnectionFull
    connections_storage(units(i).asInstanceOf[(BaseGearWheel,BaseGearWheel)])._2
  }
  def connectionsSorter: Array[GearConnection[BaseGearWheel, BaseGearWheel]] = {
    val arr_b = new ArrayBuffer[GearConnection[BaseGearWheel,BaseGearWheel]]()
    for (i <- 0 until getConnectionsAmount){
      arr_b.addOne(getConnection(i))
    }
    arr_b.toArray
  }

  override def toStringShort: String = "ChainLink, contains:" + print(gear_storage.toStringShort)
}


