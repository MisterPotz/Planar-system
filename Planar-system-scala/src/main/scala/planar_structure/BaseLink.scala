package planar_structure
import planar_structure.help_traits._
import planar_structure.core_structure._
import scala.language.implicitConversions


trait LinkManipulator {
  def getLink(i: Int): BaseLink;
}
trait ConnectionManipulator{
  def getConnection(i : Int) : GearConnection[BaseGearWheel,BaseGearWheel]
}
//basic chain of connections and wheels
class ChainLink extends Addable[BaseLink] with Updateable[BaseLink] with LinkManipulator with ConnectionManipulator
with RecognizableBaseLink {
//initializing array of mechanisms
  lazy val gear_storage: Storage[BaseLink] = new Storage[BaseLink] {}
  lazy val connections_storage : Storage[GearConnection[BaseGearWheel, BaseGearWheel]] = new Storage[GearConnection[BaseGearWheel, BaseGearWheel]] {}
  //adding new element
  override def getLink(i : Int) : BaseLink = gear_storage(i)
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
        connections_storage.addOne(GearConnection(super2SubClass[BaseGearWheel](gear_storage(i)),super2SubClass[BaseGearWheel](gear_storage(k))))
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
    else for (i <-0 until connections_storage.length) connections_storage(i).init
    this
  }
}

object Main extends App with RecognizableBaseLink {
  println("Hi")
  implicit def kek(externalConnection: ExternalConnection) : ExternalConnection = externalConnection

  val geared : ExternalConnection = new ExternalConnection(new ExternalGearWheel(), new ExternalGearWheel())
  val links = new ChainLink; links.addAll(new ExternalGearWheel(), new ExternalGearWheel(), new ExternalGearWheel(z = 200))
  //links.installConnections()
  println(links)
  val keksat : Satellite = new Satellite()

  keksat(0).add(new InternalGearWheel(z=1000))
  println(keksat)
  links.add(keksat)
  println(links)
  links.installConnections()
  println(links)
  keksat(0).getLink(0).asInstanceOf[BaseGearWheel].z =2000
  links.installConnections()
  println(links)
  /*  println(keksat.asInstanceOf[BaseLink] match {
    case k : BaseGearWheel => false
    case k : Satellite => true

  })*/
  /*println(geared match {case geared : GearConnection[BaseGearWheel, BaseGearWheel] => true; case _ => false })
  println(geared match {case geared : BaseGearWheel => true; case _ => false})*/
}
