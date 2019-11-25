package planar_structure.mechanism

import planar_structure.help_traits.BeautifulDebugOutput
//уникальные характерситики зуба, которые могут менять в пределах передачи:
//z, x, ha, ca - правильно?
object GearCalculations{
  final def mt(m : Float, beta : Float) : Float = m / math.cos(beta).toFloat
  final def xt(x : Float, beta : Float) : Float = x/math.cos(beta).toFloat
  final def hta(ha : Float, beta : Float) : Float = ha/math.cos(beta).toFloat
  final def r(m : Float, z : Int, beta: Float) : Float = d(m, z, beta) / 2 //делительный радиус
  final def rb(m : Float, z : Int, beta: Float, alpha : Float) : Float = db(m, z, beta, alpha) / 2 //радиус основной окружности
  final def db(m : Float, z : Int, beta: Float, alpha : Float) : Float = d(m, z, beta) * math.cos(alpha_t(alpha, beta)).toFloat
  final def d(m : Float, z : Int, beta : Float) : Float = m*z / math.cos(beta).toFloat//делительный диаметр
//  def da : Float //радиус вершин зубьев  //TODO
  final def alpha_t(alpha : Float, beta : Float) : Float = math.atan(math.tan(alpha) / math.cos(beta)).toFloat // угол профиля альфа_т
  final def alpha_a(m : Float, z : Int, beta: Float, alpha : Float, da : Float, ha : Float, x : Float)(calcs : WheelTypeDependentCalculations)
  : Float = math.acos(2 * rb(m, z, beta, alpha) / calcs.da(r(m,z, beta), ha, x, m)).toFloat
//  def noPruning : Boolean
}

trait WheelTypeDependentCalculations{
  def da(r : Float, ha : Float, x: Float, m : Float): Float
  def noPruning(hta : Float, xt : Float, alpha_t : Float, z : Int): Boolean
}
object InternalWheelCalculations extends WheelTypeDependentCalculations {
  override def da(r : Float, ha : Float, x: Float, m : Float): Float = (2 * r - 2 * (ha - x - 0.2) * m).toFloat
  override def noPruning(hta : Float, xt : Float, alpha_t : Float, z : Int): Boolean = true
}
object ExternalWheelCalculations extends  WheelTypeDependentCalculations {
  override def da(r : Float, ha : Float, x: Float, m : Float): Float = 2 * r + 2 * (ha + x) * m
  override def noPruning(hta : Float, xt : Float, alpha_t : Float, z : Int): Boolean = {
    val z_min = (2 * (hta - xt) / math.pow(math.sin(alpha_t), 2.0)).floor
    if (z_min> z) false else true
  }
}




//--------------------------------------------------------------------------------
trait GearGeometricCharacteristic{
  var z : Int //число зубьев
  var m: Float //модуль зацепления
  var x : Float //коэффициент смещения
  var ha: Float = 1.0f //коэффициент высоты головки зуба
  var ca: Float =0.25f //коэффициент радиального зазора
  var alpha:Float= scala.math.toRadians(20.0f).toFloat//угол профиля
  var beta : Float = 0.0f // для косозубых передач угол наклона линии зубьев
  var satelliteWheel : Boolean = false //by default all wheels are central
/*  var  axis_steady: Boolean = true //ось колеса фиксирована
  var rotates : Boolean = true*/
final def mt : Float = GearCalculations.mt(m, beta)
  final def xt: Float = GearCalculations.xt(x,beta)
  final def hta : Float = GearCalculations.hta(ha, beta)
  final def r: Float = GearCalculations.r(m,z,beta)//делительный радиус
  final def rb: Float = GearCalculations.rb(m,z,beta, alpha)//радиус основной окружности
  final def db: Float = GearCalculations.db(m,z,beta, alpha)
  final def d: Float = GearCalculations.d(m,z, beta) //делительный диаметр
  //  def da : Float //радиус вершин зубьев  //TODO
  final def alpha_t : Float = GearCalculations.alpha_t(alpha, beta) // угол профиля альфа_т
  def alpha_a: Float
  def da : Float
  def noPruning : Boolean
}
//these classes are used to hold info about the wheels. classes are used by higher structure
sealed abstract class WheelHolder //колесо не имеет связей на вращение
  extends GearGeometricCharacteristic {

}
class InternalWheelHolder extends WheelHolder {
  override var z: Int = 80
  override var m: Float = 1.25.toFloat
  override var x: Float = 0.0.toFloat
  //override def da: Float = (2 * r - 2 * (ha - x - 0.2) * m).toFloat
  def noPruning: Boolean = true
  override def alpha_a: Float = GearCalculations.alpha_a(m,z,beta, alpha, da, ha, x)(InternalWheelCalculations)
  override def da: Float = InternalWheelCalculations.da(r,ha,x,m)
  satelliteWheel = false
}
class ExternalWheelHolder extends WheelHolder{
  override var z: Int = 30
  override var m: Float = 1.25f
  override var x: Float = 0.0f
  override def noPruning: Boolean = ExternalWheelCalculations.noPruning(hta, xt, alpha_t,z)
  override def alpha_a: Float = GearCalculations.alpha_a(m,z,beta, alpha, da, ha, x)(ExternalWheelCalculations)
  override def da: Float = ExternalWheelCalculations.da(r,ha,x,m)
  satelliteWheel = false


}

object WheelHolder{
  def external : ExternalWheelHolder = new ExternalWheelHolder()
  def internal : InternalWheelHolder = new InternalWheelHolder()
  def externalSatellite : ExternalWheelHolder = new ExternalWheelHolder(){
    satelliteWheel = true
  }
  def internalSatellite : InternalWheelHolder = new InternalWheelHolder(){
    satelliteWheel = true
  }
}

//-----------------------------------
trait MaterialCharacteristic
trait GearWheel extends BeautifulDebugOutput{
  var holder: GearGeometricCharacteristic
  var material_holder: MaterialCharacteristic
}
class InternalWheel(_holder : GearGeometricCharacteristic = WheelHolder.internal, _material_holder : MaterialCharacteristic = null) extends GearWheel{

  //TODO type check for geargeometriccharacteristic
  override def toStringShort: String = holder.asInstanceOf[InternalWheelHolder].toString
  override def toString: String = toStringShort

  override var holder: GearGeometricCharacteristic = _holder
  override var material_holder: MaterialCharacteristic = _material_holder
}
class ExternalWheel(_holder : GearGeometricCharacteristic = WheelHolder.external, _material_holder: MaterialCharacteristic = null)extends GearWheel{
  override def toStringShort: String = holder.asInstanceOf[ExternalWheelHolder].toString
  override def toString: String = toStringShort
  override var holder: GearGeometricCharacteristic = _holder
  override var material_holder: MaterialCharacteristic = _material_holder
}
