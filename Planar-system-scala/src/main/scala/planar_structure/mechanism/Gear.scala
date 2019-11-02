package planar_structure.mechanism

import planar_structure.help_traits.BeautifulDebugOutput
import planar_structure.old.core_structure.links.LinkElemHolder
//уникальные характерситики зуба, которые могут менять в пределах передачи:
//z, x, ha, ca - правильно?
trait GearGeometricCharacteristic extends Characteristic{
  var z : Int //число зубьев
  var  m: Double //модуль зацепления
  var x : Double //коэффициент смещения
  var ha: Double = 1.0 //коэффициент высоты головки зуба
  var ca: Double =0.25 //коэффициент радиального зазора
  final def mt : Double = m / math.cos(beta)
  var alpha:Double=scala.math.toRadians(20.0) //угол профиля
  var beta : Double = 0.0 // для косозубых передач угол наклона линии зубьев
/*  var  axis_steady: Boolean = true //ось колеса фиксирована
  var rotates : Boolean = true*/
  final def xt : Double = x/math.cos(beta)
  final def hta : Double = ha/math.cos(beta)
  final def r : Double = d / 2 //делительный радиус
  final def rb : Double = db / 2 //радиус основной окружности
  final def db : Double = d * math.cos(alpha_t)
  final def d : Double = m*z / math.cos(beta) //делительный диаметр
  def da : Double //радиус вершин зубьев
  final def alpha_t : Double = math.atan(math.tan(alpha) / math.cos(beta)) // угол профиля альфа_т
  final def alpha_a : Double = math.acos(2 * rb / da)
  def toStringFull: String = s"z: $z\tm: $m\tx: $x\tha: $ha\tca: $ca\talpha: $alpha\t"
  def toStringShort : String = "gear wheel"
  override def toString: String = toStringShort
  def noPruning : Boolean
}
//TODO сделать da
trait MaterialCharacteristic extends  Characteristic

//these classes are used to hold info about the wheels. classes are used by higher structure
sealed abstract class WheelHolder //колесо не имеет связей на вращение
  extends GearGeometricCharacteristic {
}

class InternalWheelHolder extends WheelHolder {
  override var z: Int = 80
  override var m: Double = 1.25
  override var x: Double = 0.0
  override def da: Double = 2 * r - 2 * (ha - x - 0.2) * m;

  override def noPruning: Boolean = true
}
class ExternalWheelHolder extends WheelHolder{
  override var z: Int = 30
  override var m: Double = 1.25
  override var x: Double = 0.0
  override def da: Double = 2 * r + 2 * (ha + x) * m;

  override def noPruning: Boolean = {
    val z_min = (2 * (hta - xt) / math.pow(math.sin(alpha_t), 2.0)).ceil
    if (z_min> z) false else true
  }
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
class ExternalWheel(_holder : GearGeometricCharacteristic = WheelHolder.external, _material_holder: MaterialCharacteristic = null)extends GearWheel{
  override def toStringShort: String = holder.asInstanceOf[ExternalWheelHolder].toStringShort
  override def toString: String = toStringShort
  override var holder: GearGeometricCharacteristic = _holder
  override var material_holder: MaterialCharacteristic = _material_holder
}
