package planar_structure.core_structure

import planar_structure.help_traits.{Recognizable, Storage, Updateable}


//sealed для того, чтобы сгенерировалась enum из классов
sealed  trait  BaseLink extends Updateable{self =>
  //by default update does nothing
  override def update(): BaseLink.this.type = {self}
}

trait BaseGearConnection extends  BaseLink
//this line is done with intention to connect different types of mechanisms in future (output of one
//to output of another
trait BaseMechanism extends BaseLink
//базовая структура параметров колеса
abstract class BaseGearWheel(var z : Int,var  m: Double,var x:Double,var ha: Double = 1.0,var ca: Double =0.25,var alpha:Double=scala.math.toRadians(20.0)
                             ,var  axis_steady: Boolean = true,var rotates : Boolean = true) extends BaseLink {
  final def r : Double = m * z /2
  final def rb : Double = r * math.cos(alpha)

  override def toString: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates"

}

//базовая структура колеса и параметры с фиксированной осью и дозволенностью вращения
class ExternalGearWheel(z : Int = 30, m: Double = 1.25,x:Double = 0,
                        ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                        , axis_steady: Boolean = true, rotates : Boolean = true) extends BaseGearWheel(
  z, m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = "\nExternal gear, parameters:"+ super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_ + _)

}

class InternalGearWheel( z : Int = 30,  m: Double = 1.25,  x:Double = 0,
                         ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                         , axis_steady: Boolean = true, rotates : Boolean = true) extends BaseGearWheel(z,
  m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = "\nInternal gear, parameters:"+ super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_ + _)
}


//assumption that satellites first element is always BaseGearWheel
class Satellite extends BaseLink with RecognizableBaseLink {
  //хранит информацию о венцах и их продолджениях
  val crown_storage: Storage[ChainLink] = new Storage[ChainLink] {}
  def inputIsSet: Boolean = if (crown_storage.nonEmpty && crown_storage(0).gear_storage.nonEmpty) true else false
  def getInput: BaseGearWheel = crown_storage(0).gear_storage(0)
  override def toString: String = {
    "Satellite, contains:\n" + crown_storage.toString
  }
  def apply(index : Int): ChainLink = if(crown_storage.length == index){
      crown_storage.addOne(new ChainLink)
      crown_storage(index)
    }else {
      crown_storage(index)
  }
  //installs connection for every link
  override def update(): Satellite.this.type = {
    for (i <- crown_storage.collection) {
      i.installConnections()
    }
    this
  }
}
class Carrier extends  BaseLink
class Input extends BaseLink
class Output extends  BaseLink

trait RecognizableBaseLink extends Recognizable[BaseLink] {
  override implicit def super2SubClass[BaseGearWheel](t: BaseLink): BaseGearWheel = {
    val debug_check = if (t.isInstanceOf[Satellite]) true else false
    t match {
      case t : Satellite => {
        val gear : BaseGearWheel = if (t.inputIsSet){
          //if we need to cast Satellite into wheel, we give away the very first wheel of satellite
          super2SubClass[BaseGearWheel](t.crown_storage(0).getLink(0))
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
