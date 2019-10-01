package planar_structure.core_structure

import planar_structure.help_traits._

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions


trait LinkManipulator {
  def getLink(i: Int): BaseLink;
}
trait ConnectionManipulator{
  def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
}

trait ChainLinkInterface[T] extends Addable[T] with Updateable with LinkManipulator with ConnectionManipulator with
RecognizableBaseLink with BeautifulDebugOutput
{self =>
  override def getLink(i : Int) : BaseLink
  override def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
  //appending unique element to storages
  override def add[A <: T](a: A): ChainLinkInterface[T]
  override def addAll[A <: T](all: A*): ChainLinkInterface[T]
  //create or update connections
  def installConnections() : ChainLinkInterface[T]
  //removes old ones if there are ???not enough connections and creates new ones
  protected def installNewConnections() : ChainLinkInterface[T]
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  override def update(): ChainLinkInterface[T]
  def getBranchSize : Int
  def copy : ChainLinkInterface[T]
}
//basic chain of connections and wheels
class ChainLink(baseLink: BaseLink*) extends ChainLinkInterface[BaseLink] {
  init
//initializing array of mechanisms
  def init : Unit = {
    baseLink.foreach((f:BaseLink )=> gear_storage.addOne(f))
  }
  lazy val gear_storage: Storage[BaseLink] = new Storage[BaseLink] {}
  lazy val connections_storage : StorageHashD[(BaseGearWheel, BaseGearWheel), GearConnection[BaseGearWheel,BaseGearWheel]]
    = new StorageHashD[(BaseGearWheel, BaseGearWheel), GearConnection[BaseGearWheel,BaseGearWheel]] {}
  protected def getNextTo(link: BaseLink) : BaseLink = {
    var baseLink : BaseLink = null
    for (i <- 0 until gear_storage.collection.length-1)
      if (gear_storage.collection(i) eq link) {
        baseLink =  gear_storage.collection(i+1)
      }
    baseLink
  }
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
      case (i : BaseGearWheel, k: Satellite) => true
      case _ => false}
  }
  def allowedForConnection(baseLink: BaseLink) : Boolean = {
    baseLink match {
      case i : BaseGearWheel => true
      case i : Satellite => true
      case _ => false
    }
  }
  def getAllLinksAllowedForConnection : Array[BaseLink] = {
    gear_storage.collection.foldLeft[ArrayBuffer[BaseLink]](new ArrayBuffer())(
      (left : ArrayBuffer[BaseLink], right :BaseLink) => {
        if (left.nonEmpty && allowedForConnection(left.last, right)) {
          left.addOne(right)
          left
        }else  if(left.isEmpty && allowedForConnection(right)){
          left.addOne(right)
        } else{
          left
        }
      }
    ).toArray
  }
  //TODO counter for connections - for each chainlink scrape allowed connections - so no additional checks needed
  // and then search through them each connection, with counter in mind, so needed connection will be found
  override def getConnection(i: Int): GearConnection[BaseGearWheel, BaseGearWheel] = {
    val allowed_links = getAllLinksAllowedForConnection
    if (connections_storage.length -1 <= i)
      //assumption that the last allowed for connection element is satellite
      //TODO add to satellite the way to find connections
      allowed_links.last.getConnection(i - connections_storage.length)
    else
      connections_storage.collection((allowed_links(i),allowed_links(i+1)))
      //go to next chainlinks
  }

  def getConnectionsAmount : Int ={
    var all_connections : IndexedSeq[GearConnection[BaseGearWheel, BaseGearWheel]] = null
    try {
       all_connections  = {
        for (i <- 0 until getBranchSize-3) yield getConnection(i)
      }
    } catch{
      case _ => println("Something happened")
    }
    all_connections.length
  }
  //TODO create mechanism from file (JSON)
  override def toString: String = "\nChain link:" + print("\ngears:\n" +gear_storage.toString + "\n" + "connections:\n"+ connections_storage.toString)
  //appending unique element to storages
  def add[A <: BaseLink](a: A): ChainLink = {
    a match {
      case a : BaseGearWheel => gear_storage.addOne(a)
      case a : Satellite => gear_storage.addOne(a)
      case _ => println("Element was declined, not appended")
    }
    this
  }
  def addAll[A <: BaseLink](all: A*): ChainLink = {
    for (a <- all) add(a)
    this
  }
  //create or update connections
  def installConnections() : ChainLink  = update()
  //removes old ones if there are not enough connections and creates new ones
  protected def installNewConnections() : ChainLink= {
    connections_storage.collection.clear()
    //just creating new array with certified baselinks to be sure that no bad things will happen
    val gears_to_connect = getAllLinksAllowedForConnection
    for (i <- 0 until gears_to_connect.length - 1)
    connections_storage.addOne(((gear_storage(i), gear_storage(i+1)), GearConnection((gear_storage(i)),(gear_storage(i+1)))))
    this
  }
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  override def update(): ChainLink = {
    val is_equipped = if (connections_storage.length < gear_storage.length - 1) false else true
    //delete old connections if there are less of them than it should
    if (!is_equipped) {
      installNewConnections()
    }
    //or just reinit them if there are as much connections as it should
    else {
      for ( i <- 0 until connections_storage.length) {
        connections_storage((gear_storage(i), gear_storage(i+1)))._2.init
        //basically this does nothing but in case of satellite - then it updates satellites connections
        //so a command from one point re-initializes connections all over the rest mechanism
      }
    }
    for (i <- gear_storage.collection){
      i.update()
    }
    this
  }

  override def getBranchSize: Int = {
    gear_storage.collection.foldLeft(0)((left : Int, right: BaseLink) => left + right.getBranchSize)
  }
  override def copy : ChainLink  ={
    val chainLink : ChainLink = new ChainLink()
    for (i <- 0 until gear_storage.length)
      chainLink.add(gear_storage(i).copy)
    chainLink
  }
}


