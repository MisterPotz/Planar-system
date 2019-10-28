package planar_structure.old.core_structure.links

import planar_structure.old.core_structure.LinkSeq
import planar_structure.help_traits.BeautifulDebugOutput

import scala.collection.mutable
abstract class AbstractHolder
//used to create more advanced storages of case structures
abstract class LinkElemHolder extends AbstractHolder

//these classes are used to hold info about the wheels. classes are used by higher structure
sealed abstract class WheelHolder(var z : Int, //число зубьев
                                   var  m: Double //модуль зацепления
                                  ,var x:Double, //коэффициент смещения
                                  var ha: Double = 1.0, //коэффициент высоты головки зуба
                                  var ca: Double =0.25, //коэффициент радиального зазора
                                  var alpha:Double=scala.math.toRadians(20.0), //угол профиля
                                  var beta : Double = 0.0, // для косозубых передач, угол наклона линии зубьев
                                   var  axis_steady: Boolean = true, //ось колеса фиксирована
                                  var rotates : Boolean = true) //колесо не имеет связей на вращение
  extends LinkElemHolder with BeautifulDebugOutput {
  final def xt : Double = x/math.cos(beta)
  final def hta : Double = ha/math.cos(beta)
  final def r : Double = d / 2 //делительный радиус
  final def rb : Double = db / 2 //радиус основной окружности
  final def db : Double = d * math.cos(alpha_t)
  final def d : Double = m*z / math.cos(beta) //делительный диаметр
  final def alpha_t : Double = math.atan(math.tan(alpha) / math.cos(beta)) // угол профиля альфа_т
  override def toStringFull: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates\n"
  override def toStringShort : String = "gear wheel"
  override def toString: String = toStringShort
}

class InternalWheelHolder(z : Int = 30, m: Double = 1.25,x:Double = 0,
                          ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0), beta : Double = 0.0
                          , axis_steady: Boolean = true, rotates : Boolean = true) extends WheelHolder(
  z, m,x,ha,ca,alpha, beta, axis_steady, rotates) {
  override def toStringFull: String = BeautifulDebugOutput.print("Internal gear, parameters:\n"+ super.toStringFull)
  override def toStringShort : String = "Internal " + super.toStringShort
  override def toString: String = toStringShort
  def copy: InternalWheelHolder = new InternalWheelHolder(z,m,x,ha,ca,alpha,beta, axis_steady,rotates)
}

class ExternalWheelHolder(z : Int = 30, m: Double = 1.25,x:Double = 0,
                          ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0), beta : Double = 0.0
                          , axis_steady: Boolean = true, rotates : Boolean = true) extends WheelHolder(
  z, m,x,ha,ca,alpha, beta, axis_steady, rotates) {
  override def toStringFull: String = BeautifulDebugOutput.print("External gear, parameters:\n"+ super.toStringFull)
  override def toStringShort : String = "External " + super.toStringShort
  def copy: InternalWheelHolder = new InternalWheelHolder(z,m,x,ha,ca,alpha,beta, axis_steady,rotates)
  override def toString: String = toStringShort
}

object WheelHolder{
  def external : ExternalWheelHolder = new ExternalWheelHolder()
  def internal : InternalWheelHolder = new InternalWheelHolder()
}

class SatelliteHolder(val crowns : mutable.HashMap[Int, LinkSeq] = mutable.HashMap.empty[Int, LinkSeq], val satellitesAmount: Int = 3) extends LinkElemHolder

trait LinkHolderImplicits{
  implicit def hashMap2Holder(map : mutable.HashMap[Int, LinkSeq]) : SatelliteHolder = {
    new SatelliteHolder(map)
  }
}
