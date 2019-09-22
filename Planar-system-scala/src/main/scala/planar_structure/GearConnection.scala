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

class ExternalConnection(first : ExternalGearWheel, second: ExternalGearWheel) extends GearConnection(first, second){
  super.init
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w = (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  override def updateAllRw(): Unit = {rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)}
  override def updateAw(): Unit = aw = rw1 + rw2

  override def toString: String =  "\nexternal connection, params" +super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_+_)
}

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
  implicit def twoGears2ExtGearConnection(first : BaseGearWheel, second : BaseGearWheel) : ExternalConnection ={
    val baseGearWheels = List(first, second)
    if (baseGearWheels.length > 2 & baseGearWheels == 0)
      throw new IllegalArgumentException("can't construct External Connection: invalid length")
    for (baseWheel <- baseGearWheels){
      baseWheel match {
        case baseWheel : ExternalGearWheel => true
        case _  => throw new ClassCastException("can't construct External Connection: illegal type")
      }
    }
    new ExternalConnection(baseGearWheels(0).asInstanceOf[ExternalGearWheel], baseGearWheels(1).asInstanceOf[ExternalGearWheel])
  }
  //checking that passed arguments have only one ExternalGear and one Internal and then passing them to
  //constructor of Internal Connection
  implicit def twoGears2IntGearConnection(first : BaseGearWheel, second : BaseGearWheel) : InternalConnection ={
    val baseGearWheels = List(first, second)
    if (baseGearWheels.length > 2 & baseGearWheels == 0)
      throw new IllegalArgumentException("can't construct Internal Connection: invalid length")
    if (baseGearWheels.count( (p:BaseGearWheel) => p match {
      case p: InternalGearWheel => true
      case _ => false
    }) == 1 & baseGearWheels.count((p : BaseGearWheel) => p match {
      case p: ExternalGearWheel => true
      case _ => false
    }) == 1 ) {
      new InternalConnection(baseGearWheels(0),  baseGearWheels(1))
    }
    else
      throw new ClassCastException("can't construct External Connection: illegal type")
  }
}
//companion объект для соединений колёс
object GearConnection extends GearConnectionCreator {

  //static функции для нахождения необходимых величин

}
