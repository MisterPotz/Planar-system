package planar_structure.core_structure

import planar_structure.help_traits.{BeautifulDebugOutput, Recognizable, StorageHashD, StorageHashDConnection}

import scala.language.implicitConversions
import scala.math.cos
//TODO внедрить владовские наработки
//helper trait (interface) to deal with involute finding problems
trait GearObjectedConversions{
  implicit class InvToRad(i : Double) {
    self =>
    //подключение к классу Double дополнительных полезных функций
    //нахождение угла по инволюте обратной функцией инволюты по методу Ченга
    //TODO ограничить допустимый угол для использования функции ченга
    def invToRad: Double = math.pow(3 * i, 1 / 3.0) - (2 * i) / 5.0 + (9 / 175.0) * math.pow(3, 2 / 3.0) *
      math.pow(i, 5 / 3.0) - (2 / 175.0) * math.pow(3, 1 / 3.0) * math.pow(i, 7 / 3.0) - (144 / 67375.0) * math.pow(i, 3.0) +
      (3258 / 3128125.0) * math.pow(3, 2 / 3.0) * math.pow(i, 11 / 3.0) - (49711 / 153278125.0) * math.pow(3, 1 / 3.0) *
      math.pow(i, 13 / 3.0)

    //нахождение инволюты по углу
    def radToInv: Double = math.tan(i) - i
  }
}
object GearConnection extends RecognizableConnection with GearConnectionCreator{
  def apply(first: BaseGearWheel, second: BaseGearWheel): GearConnection[BaseGearWheel, BaseGearWheel] ={
    makeGearConnection(first, second)
  }
  val connection_storage : StorageHashDConnection =
    new StorageHashDConnection {}

}
abstract class GearConnection[+T <: BaseGearWheel, +T2 <: BaseGearWheel](val first : T,val second : T2) extends GearObjectedConversions
with BaseGearConnection with BeautifulDebugOutput {
  //инициализация
  init
  def init  = {
    //перерасчет alpha_w
    updateAlpha_w()
    //перерасчет всех Rw
    updateAllRw()
    //перерасчет Aw
    updateAw()
  }
  var alpha_w : Double = 0.0
  var rw1 : Double = 0.0
  var rw2 : Double = 0.0
  var aw : Double = 0.0
  def updateAlpha_w()
  def findRw(alpha : Double, m :Double, z: Double) : Double
  def updateAllRw()
  def updateAw()
  override def toString: String = s"alpha_w: $alpha_w\nrw1: $rw1\nrw2: $rw2\naw: $aw"
  def toStringShort : String = ""
  override def getBranchSize : Int = 1
}
object ExternalConnection
class ExternalConnection( first : ExternalGearWheel, second: ExternalGearWheel) extends GearConnection(first, second){
  super.init
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w = (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  override def updateAllRw(): Unit = {rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)}
  override def updateAw(): Unit = aw = rw1 + rw2

  override def copy: BaseLink = new ExternalConnection(first, second)
  override def toString: String =  print("External connection:\n" +super.toString)

  override def toStringShort: String = "External connection" concat super.toStringShort
}
object InternalConnection
class InternalConnection(first : BaseGearWheel, second: BaseGearWheel) extends GearConnection(first, second){
  super.init
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w = (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  override def updateAllRw(): Unit = {rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)}
  override def updateAw(): Unit = aw = rw1 + rw2
  override def toString: String =  print("Internal connection:\n" +super.toString)
  override def toStringShort: String = "Internal connection" concat super.toStringShort

  override def copy: BaseLink = new InternalConnection(first, second)
  //TODO вбить правильные формулы для внутреннего зацепления
}

trait GearConnectionCreator {
  def recognizeConnectionType(first: BaseGearWheel, second: BaseGearWheel): Option[GearConnection[BaseGearWheel, BaseGearWheel]] = {
    (first, second) match {
      case (first: ExternalGearWheel, second: ExternalGearWheel) => Some(new ExternalConnection(first, second))
      case (first: ExternalGearWheel, second: InternalGearWheel) => Some(new InternalConnection(first, second))
      case (first: InternalGearWheel, second: ExternalGearWheel) => Some(new InternalConnection(first, second))
      case _ => None
    }
  }
  //сделать соединение
  def makeGearConnection(first: BaseGearWheel, second: BaseGearWheel): GearConnection[BaseGearWheel, BaseGearWheel] = {
    val recognized_type = recognizeConnectionType(first, second)
    recognized_type match {
      //uses implicit case from basegearwheel to external gearwheel
      case Some(kek: ExternalConnection) => kek
      case Some(kek: InternalConnection) => kek
      case None => throw new IllegalArgumentException("Connection can't be created")
    }
  }
}


trait RecognizableConnection extends Recognizable[GearConnection[BaseGearWheel, BaseGearWheel]]{
  override implicit def super2SubClass[ExternalConnection](t: GearConnection[BaseGearWheel, BaseGearWheel]): ExternalConnection = {
    t match {
      case t : ExternalConnection => t
      case _ => throw new ClassCastException("Can't create Ext Connection from Connection")
    }
  }

 /* implicit def sub2Option(t: GearConnection[BaseGearWheel, BaseGearWheel]): Option[GearConnection[BaseGearWheel, BaseGearWheel]] = {
    t match {
      case t : ExternalConnection => Some(t)
      case t : InternalConnection => Some(t)
      case _ => None
    }
  }*/
}
