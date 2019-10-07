package planar_structure.core_structure.connections

import planar_structure.core_structure.links.{ExternalWheelHolder, LinkElemHolder, WheelHolder}
import planar_structure.help_traits.BeautifulDebugOutput

import scala.math._

//is used to calculate different useful things
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

sealed abstract class ConnectionHolder(first_ : LinkElemHolder, second_ : LinkElemHolder) extends BeautifulDebugOutput {
  def first : LinkElemHolder = first_
  def second : LinkElemHolder = second_
  //initialize connection based on input
  def init() : Unit = {}
}

trait WheelConnectionCalculator extends GearObjectedConversions {
  def updateAlpha_w()
  def findRw(alpha: Double, m: Double, z: Double): Double
  def updateAllRw()
  def updateAw()
  var alpha_w: Double = 0.0
  var rw1: Double = 0.0
  var rw2: Double = 0.0
  var aw: Double = 0.0
}

sealed abstract class GearConnectionHolder(first_ : WheelHolder, second_ : WheelHolder)
  extends ConnectionHolder(first_, second_)
     with WheelConnectionCalculator {
  override def first: WheelHolder = first_
  override def second: WheelHolder = second_
  //инициализация
  init()
  override def init(): Unit = {
    //перерасчет alpha_w
    updateAlpha_w()
    //перерасчет всех Rw
    updateAllRw()
    //перерасчет Aw
    updateAw()
  }
  override def toStringFull: String = s"alpha_w: $alpha_w\nrw1: $rw1\nrw2: $rw2\naw: $aw"
  override def toString: String = toStringShort
  override def toStringShort : String = s"connection of (${first.toStringShort}, ${second.toStringShort})"

}

class InternalConnectionHolder(first_ : WheelHolder, second_ : WheelHolder)
  extends GearConnectionHolder(first_, second_){

  super.init()
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w ={
    (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  }
  override def updateAllRw(): Unit = {
    rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)
  }
  override def updateAw(): Unit = aw = rw1 + rw2
  override def toStringFull: String =  BeautifulDebugOutput.print("Internal connection:\n" +super.toString)
  override def toStringShort: String = "Internal " concat super.toStringShort
  def copy: InternalConnectionHolder = new InternalConnectionHolder(first, second)
  override def toString: String = toStringShort
}

class ExternalConnectionHolder(first : ExternalWheelHolder, second : ExternalWheelHolder)
  extends GearConnectionHolder(first, second){
  super.init()
  override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  override def updateAlpha_w(): Unit = alpha_w ={
    (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  }
  override def updateAllRw(): Unit = {
    rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)
  }
  override def updateAw(): Unit = aw = rw1 + rw2
  override def toStringFull: String =  BeautifulDebugOutput.print("External connection:\n" +super.toString)
  override def toStringShort: String = "External " concat super.toStringShort
  def copy: ExternalConnectionHolder = new ExternalConnectionHolder(first, second)
  override def toString: String = toStringShort
}

