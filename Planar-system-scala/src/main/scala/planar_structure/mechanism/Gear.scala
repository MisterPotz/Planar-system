package planar_structure.mechanism

import planar_structure.help_traits.BeautifulDebugOutput
import planar_structure.old.core_structure.links.LinkElemHolder

trait GearGeometricCharacteristic extends Characteristic{
  var z : Int //число зубьев
  var  m: Double //модуль зацепления
  var x:Double //коэффициент смещения
  var ha: Double = 1.0 //коэффициент высоты головки зуба
  var ca: Double =0.25 //коэффициент радиального зазора
  var alpha:Double=scala.math.toRadians(20.0) //угол профиля
  var beta : Double = 0.0 // для косозубых передач угол наклона линии зубьев
  var  axis_steady: Boolean = true //ось колеса фиксирована
  var rotates : Boolean = true
  final def xt : Double = x/math.cos(beta)
  final def hta : Double = ha/math.cos(beta)
  final def r : Double = d / 2 //делительный радиус
  final def rb : Double = db / 2 //радиус основной окружности
  final def db : Double = d * math.cos(alpha_t)
  final def d : Double = m*z / math.cos(beta) //делительный диаметр
  final def alpha_t : Double = math.atan(math.tan(alpha) / math.cos(beta)) // угол профиля альфа_т
  def toStringFull: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates\n"
  def toStringShort : String = "gear wheel"
  override def toString: String = toStringShort
}
trait MaterialCharacteristic extends  Characteristic

//these classes are used to hold info about the wheels. classes are used by higher structure
sealed abstract class WheelHolder //колесо не имеет связей на вращение
  extends GearGeometricCharacteristic with BeautifulDebugOutput {
}

class InternalWheelHolder extends WheelHolder {
  override var z: Int = _
  override var m: Double = _
  override var x: Double = _
}
class ExternalWheelHolder extends WheelHolder{
  override var z: Int = _
  override var m: Double = _
  override var x: Double = _
}

object WheelHolder{
  def external : ExternalWheelHolder = new ExternalWheelHolder()
  def internal : InternalWheelHolder = new InternalWheelHolder()
}


trait GearWheel extends BeautifulDebugOutput{
  var holder: GearGeometricCharacteristic
  var material_holder: MaterialCharacteristic
}
class InternalWheel(_holder : GearGeometricCharacteristic = WheelHolder.internal, _material_holder : MaterialCharacteristic = null) extends GearWheel{

  //TODO type check for geargeometriccharacteristic
  override def toStringShort: String = holder.asInstanceOf[InternalWheelHolder].toStringShort
  override def toString: String = toStringShort

  override var holder: GearGeometricCharacteristic = _holder
  override var material_holder: MaterialCharacteristic = _material_holder
}
class ExternalWheel(_holder : GearGeometricCharacteristic = WheelHolder.internal, _material_holder: MaterialCharacteristic = null)extends GearWheel{
  override def toStringShort: String = holder.asInstanceOf[ExternalWheelHolder].toStringShort
  override def toString: String = toStringShort
  override var holder: GearGeometricCharacteristic = _holder
  override var material_holder: MaterialCharacteristic = _material_holder
}
