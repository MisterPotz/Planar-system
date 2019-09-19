package planar_structure
import scala.language.implicitConversions
import scala.math.{Pi, cos, sin, toRadians}

//helper trait (interface) to deal with involute finding problems
trait GearObjectedConversions{
  implicit class InvToRad(i : Double){
      //подключение к классу Double дополнительных полезных функций
    def invToRad : Double = math.pow(3*i, 1/3.0) - (2*i)/5.0 + (9/175.0)*math.pow(3, 2/3.0)*
      math.pow(i, 5/3.0) - (2/175.0)*math.pow(3, 1/3.0)*math.pow(i, 7/3.0)-(144/67375.0)*math.pow(i,3.0)+
      (3258/3128125.0)*math.pow(3, 2/3.0)*math.pow(i, 11/3.0) - (49711/153278125.0)*math.pow(3,1/3.0)*
      math.pow(i, 13/3.0)
    def radToInv : Double = math.tan(i) - i
  }
}

class GearConnection(first : BaseGearWheel, second : BaseGearWheel) extends GearObjectedConversions {
  //инициализация
  init
  def init : Unit = {
    updateAlpa_w()
    updateRw(first.alpha, first.m, first.z)
    updateRw(second.alpha, second.m, second.z)
  }
  var alpha_w : Double = 0.0
  var rw1 : Double = 0.0
  var rw2 : Double = 0.0
  var aw : Double = 0.0
  def updateAlpa_w():Unit = alpha_w = (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  def updateRw(alpha : Double, m :Double, z: Double) : Unit = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
  def updateAw : Unit = aw = rw1 + rw2
}
//companion объект для соединений колёс
object GearConnection{
  var alpha_w : Double = 0.0

  //для наявного создания упаковки под оба колеса соединения
  implicit def twoGears2GearConnection(first : BaseGearWheel, second : BaseGearWheel) : GearConnection = {
    GearConnection(first, second)
  }
  //функции для нахождения необходимых величин
}
