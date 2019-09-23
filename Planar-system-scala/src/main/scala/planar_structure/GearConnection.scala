package planar_structure
import scala.language.implicitConversions
import scala.math.{Pi, cos, sin, toRadians}

//helper trait (interface) to deal with involute finding problems
trait GearObjectedConversions{
  implicit class InvToRad(i : Double){self =>
      //подключение к классу Double дополнительных полезных функций
    //нахождение угла по инволюте обратной функцией инволюты по методу Ченга
    //TODO ограничить допустимый угол для использования функции ченга
    def invToRad : Double = math.pow(3*i, 1/3.0) - (2*i)/5.0 + (9/175.0)*math.pow(3, 2/3.0)*
      math.pow(i, 5/3.0) - (2/175.0)*math.pow(3, 1/3.0)*math.pow(i, 7/3.0)-(144/67375.0)*math.pow(i,3.0)+
      (3258/3128125.0)*math.pow(3, 2/3.0)*math.pow(i, 11/3.0) - (49711/153278125.0)*math.pow(3,1/3.0)*
      math.pow(i, 13/3.0)
    //нахождение инволюты по углу
    def radToInv : Double = math.tan(i) - i
  }

}

abstract class GearConnection[+T <: BaseGearWheel, +T2 <: BaseGearWheel](first : T, second : T2) extends GearObjectedConversions
with BaseGearConnection {
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
  //TODO сделать шаблон для GearConnection (implicit или стандарт) чтобы при разных колесах разные алгоритмы брались
  def updateAlpha_w()
  def findRw(alpha : Double, m :Double, z: Double) : Double
  def updateAllRw()
  def updateAw()
  override def toString: String = s"alpha_w: $alpha_w\nrw1: $rw1\nrw2: $rw2\naw: $aw"
}

object ExternalConnection
class ExternalConnection(first : ExternalGearWheel, second: ExternalGearWheel) extends GearConnection(first, second){
  super.init
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w = (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  override def updateAllRw(): Unit = {rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)}
  override def updateAw(): Unit = aw = rw1 + rw2

  override def toString: String =  "\nexternal connection, params" +super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_+_)
}
object InternalConnection
class InternalConnection(first : BaseGearWheel, second: BaseGearWheel) extends GearConnection(first, second){
  super.init
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w = (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  override def updateAllRw(): Unit = {rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)}
  override def updateAw(): Unit = aw = rw1 + rw2
  override def toString: String =  "\ninternal connection, params" +super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_+_)

  //TODO вбить правильные формулы для внутреннего зацепления
}

trait GearConnectionCreator{
  //для наявного создания упаковки под оба колеса соединения
  def isExternal(first : BaseGearWheel, second : BaseGearWheel) : Boolean = {
    val baseGearWheels = List(first, second)
    if (baseGearWheels.length > 2 & baseGearWheels == 0)
      throw new IllegalArgumentException("can't construct External Connection: invalid length")
    if (baseGearWheels.count( (p : BaseGearWheel)=> {p match {
        case p: ExternalGearWheel => true
        case _ => false
      }}
     ) == 2) true
    else false
  }
  def isInternal(first : BaseGearWheel, second : BaseGearWheel) : Boolean = {
    val baseGearWheels = List(first, second)
    if (baseGearWheels.length > 2 & baseGearWheels == 0)
      throw new IllegalArgumentException("can't construct Internal Connection: invalid length")
    if (baseGearWheels.count( (p:BaseGearWheel) => p match {
      case p: InternalGearWheel => true
      case _ => false
    }) == 1 & baseGearWheels.count((p : BaseGearWheel) => p match {
      case p: ExternalGearWheel => true
      case _ => false
    }) == 1 ) true
    else false
  }

  def recognizeType(first : BaseGearWheel, second : BaseGearWheel) : Option[_] = {
    if (isExternal(first, second)) Some(ExternalConnection)
    else if (isInternal(first, second)) Some(InternalConnection)
    else None
  }
  //сделать соединение
  def makeGearConnection(first : BaseGearWheel, second : BaseGearWheel) : GearConnection[BaseGearWheel,BaseGearWheel] = {
    val recognized_type = recognizeType(first, second)
    recognized_type match {
      case Some(ExternalConnection) => new ExternalConnection(first.asInstanceOf[ExternalGearWheel], second.asInstanceOf[ExternalGearWheel])
      case Some(InternalConnection) => new InternalConnection(first, second)
      case None => throw new IllegalArgumentException("Connection can't be created")
    }
  }
}
//companion объект для соединений колёс
object GearConnection extends GearConnectionCreator {

  //static функции для нахождения необходимых величин

}
