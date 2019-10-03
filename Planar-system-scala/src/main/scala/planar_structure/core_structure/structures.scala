package planar_structure.core_structure

import planar_structure.help_traits.{BeautifulDebugOutput, Recognizable, Storage, StorageHash, StorageHashDConnection, Updateable}

import scala.collection.mutable.ListBuffer
trait Storable{
  def toStringShort : String
}
//sealed для того, чтобы сгенерировалась enum из классов
sealed  trait  BaseLink extends Updateable with Storable{self =>
  //by default update does nothing
  override def update(): BaseLink.this.type = {self}
  def getBranchSize : Int = 1
  def getLink(index : Int) : BaseLink = {this}
  def copy : BaseLink
}

trait BaseGearConnection extends  BaseLink
//this line is done with intention to connect different types of mechanisms in future (output of one
//to output of another
trait BaseMechanism extends BaseLink
//базовая структура параметров колеса
abstract class BaseGearWheel(var z : Int,var  m: Double,var x:Double,var ha: Double = 1.0,var ca: Double =0.25,var alpha:Double=scala.math.toRadians(20.0)
                             ,var  axis_steady: Boolean = true,var rotates : Boolean = true) extends BaseLink with BeautifulDebugOutput {
  final def r : Double = m * z /2
  final def rb : Double = r * math.cos(alpha)

  override def toString: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates"
  override def toStringShort : String = "gear wheel"
  override def getBranchSize: Int = 1
}

//базовая структура колеса и параметры с фиксированной осью и дозволенностью вращения
class ExternalGearWheel(z : Int = 30, m: Double = 1.25,x:Double = 0,
                        ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                        , axis_steady: Boolean = true, rotates : Boolean = true) extends BaseGearWheel(
  z, m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = print("External gear, parameters:\n"+ super.toString)
  override def toStringShort : String = "External " + super.toStringShort
  override def copy: ExternalGearWheel = new ExternalGearWheel(z,m,x,ha,ca,alpha,axis_steady,rotates)

}

class InternalGearWheel( z : Int = 30,  m: Double = 1.25,  x:Double = 0,
                         ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                         , axis_steady: Boolean = true, rotates : Boolean = true) extends BaseGearWheel(z,
  m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = print("Internal gear, parameters:\n"+ super.toString)
  override def copy: InternalGearWheel = new InternalGearWheel(z,m,x,ha,ca,alpha,axis_steady,rotates)
  override def toStringShort : String = "Internal " + super.toStringShort

}

//assumption that satellites first element is always BaseGearWheel
class Satellite(protected val connections_storage :StorageHashDConnection, val crown_storage : StorageHash[ChainLink] = new StorageHash[ChainLink] {}) extends BaseLink with RecognizableBaseLink with BeautifulDebugOutput {
  val carrierChainLink : ChainLink = new ChainLink(connections_storage)
  override def copy: Satellite = {
    val new_storage = new StorageHash[ChainLink]{}
    for (i <- crown_storage.collection){
      new_storage.addOne((i._1, i._2.copy))
    }
    new Satellite(connections_storage,new_storage)
  }

  //хранит информацию о венцах и их продолджениях
/*
  val crown_storage: StorageHash[ChainLink] = new StorageHash[ChainLink] {}
*/
  //input satellites gear wheel, by default it's zero layer, so we allow both negative and positive numbers
  protected lazy val crown_storage_input: (ChainLink) = crown_storage(0)._2
  def inputIsSet: Boolean = if (crown_storage.nonEmpty && crown_storage_input.gear_storage.nonEmpty) true else false
  def getInput: BaseGearWheel = crown_storage_input.gear_storage(0)
  override def toString: String = {
    "Satellite, contains:" + print(crown_storage.toString)
  }
  def toStringShort : String = s"Satellite, ${crown_storage.length} crown(s): " + print(crown_storage.toStringShort)
  def addChainLink(index : Int,t :  BaseLink*): Satellite = {
    val chainLink : ChainLink = new ChainLink(connections_storage)
    for (tx <- t){
      chainLink.add(tx)
    }
    crown_storage.addOne(index, chainLink)
    this
  }
  def addChainLink(index : Int,t : ChainLink): Satellite = {
    crown_storage.addOne(index, t)
    this
  }
  def addChainLinks(elems: (Int, ChainLink)*) : Satellite.this.type = {
    elems.foreach((f:(Int, ChainLink))=>crown_storage.collection.addOne(f))
    this
  }
  def apply(index : Int): ChainLink =
    if (crown_storage.collection.contains(index))
      crown_storage(index)._2
    else {
      throw new IllegalArgumentException("No such index fr satellites crown wheel")
    }
  override def getBranchSize: Int = {
    crown_storage.collection.foldLeft[Int](0)((t : Int, b : (Int, ChainLink)) => {t + b._2.getBranchSize} )
  }
  //installs connection for every link
  override def update(): Satellite.this.type = {
    for (i <- crown_storage.collection) {
      i._2.installConnections()
    }
    this
  }
  protected def getLinkRec(index : Int, current: Iterator[Int]) : BaseLink = {
    val current_index = current.next()
      if ( crown_storage.collection(current_index).getBranchSize - 1  >= index){
        crown_storage.collection(current_index).getLink(index)
      }
      else
        getLinkRec(index-crown_storage.collection(current_index).getBranchSize, current)
  }
  protected def availableIndeces : Array[Int] = {
    val sorted_indeces : Array[Int] = crown_storage.collection.keys.toArray.filter(_ != 0)
    Array(0) ++ sorted_indeces.sorted
  }
  override def getLink(index: Int): BaseLink = {
    getLinkRec(index, availableIndeces.toIterable.iterator)
  }
  def getConnection(index: Int) : GearConnection[BaseGearWheel, BaseGearWheel] = ???
  def getConnectionStorage : StorageHashDConnection = connections_storage
  def getAllLinksAllowedForConnectionFull : ListBuffer[(BaseLink,BaseLink)] = {
    val indeces = availableIndeces
    val returning_list = new ListBuffer[(BaseLink,BaseLink)]
    for (i <- indeces) {
      returning_list.appendAll(crown_storage.collection(i).getAllLinksAllowedForConnectionFull)
    }
    returning_list
  }
  def addCarrierChainLink(baseLink: BaseLink*) : Satellite = {
    baseLink.foreach(carrierChainLink.add(_))
    this
  }

}
class Carrier extends  BaseLink{
  override def copy: Carrier = new Carrier
  override def getBranchSize: Int = 1

  override def toStringShort: String = "Carrier"
}
class Input extends BaseLink{
  override def copy: Input = new Input
  override def getBranchSize: Int = 1

  override def toStringShort: String = "Input"
}
class Output extends  BaseLink{
  override def copy: Output= new Output
  override def getBranchSize: Int = 1

  override def toStringShort: String = "Output"
}

trait RecognizableBaseLink extends Recognizable[BaseLink] {
  override implicit def super2SubClass[BaseGearWheel](t: BaseLink): BaseGearWheel = {
    t match {
      case t : Satellite => {
        val gear : BaseGearWheel = if (t.inputIsSet){
          //if we need to cast Satellite into wheel, we give away the very first wheel of satellite
          super2SubClass[BaseGearWheel](t.getInput)
        }
        else throw new ClassCastException("Can't create BaseGearWheel from Satellite")
        //then we install connections for every other mechanims in the chain
        t.update()
        gear
      }
      case u : Carrier => {println("what"); new ExternalGearWheel().asInstanceOf[BaseGearWheel]}
        //must have the lowest position in the case list, type equals to the returned one
      case t : BaseGearWheel => t
      case _ => throw new ClassCastException("Can't create BaseGearWheel from BaseLink")
    }
  }
  override implicit def sub2Option(t: BaseLink): Option[BaseLink] = {
    t match {
      case t : BaseGearWheel => Some(t)
      case t : Satellite => Some(t)
      case t : Input => Some(t)
      case t : Output => Some(t)
      case t : Carrier => Some(t)
      case _ => None
    }
  }
}
trait RecognizableBaseGearWheel extends Recognizable[BaseGearWheel]{
  override implicit def super2SubClass[ExternalGearWheel](t: BaseGearWheel): ExternalGearWheel = {
    t match {case t : ExternalGearWheel => t; case _ => throw new ClassCastException("Can't create External gear from BaseGearWheel")}
  }
  override implicit def sub2Option(t: BaseGearWheel): Option[BaseGearWheel] = {
    t match {
      case t : ExternalGearWheel => Some(t)
      case t : InternalGearWheel => Some(t)
      case _ => None
    }
  }
}
