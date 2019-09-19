package planar_structure
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer
//sealed для того, чтобы сгенерировалась enum из классов
sealed trait BaseLink {
  //следующее звено цепи
  protected var _nextLink : BaseLink = null
  //предыдущее звено цепи
  protected var _prevLink : BaseLink = null
  def nextLink : BaseLink = _nextLink
  def nextLink_=(arg : BaseLink) : Unit = if (this eq arg) throw new IllegalArgumentException("Self-shortening chain") else _nextLink = arg
  def prevLink : BaseLink = _prevLink
  def prevLink_=(value : BaseLink) : Unit = if (this eq value) throw new IllegalArgumentException("Self-shortening chain") else _prevLink = value
  //эксцентриситет центра элемента относительно оси в плоскости оси
  protected def _eccentricity : Double
  //высота элемента в направлении, перпендикулярном оси, в плоскости оси, от начала до конца (от центра, находящегося на
  //ээксцентриситете _eccentricity до сопряжения с другим элементом
  protected def _linkLength : Double
}

trait Cloneable[T] {self: T =>
   def cloneGear : T
}

class BaseGearWheel extends BaseLink with Cloneable[BaseGearWheel] {
  //число зубьев
  var z : Int = 0
  //модуль колеса
  var m : Double = 0
  //смещение инструмента
  var x : Double = 0
  //коэффициент головки зуба
  var ha : Double = 1.0
  //коэффициент зазора
  var ca: Double = 0.25
  //угол исходного профиля ИПК, в радианах
  var alpha: Double = scala.math.toRadians(20)
  def r : Double = m * z / 2
  def rb: Double = r * math.cos(alpha)
  def cloneGear:BaseGearWheel = new BaseGearWheel{z = this.z; m = this.m; x = this.x; ha = this.ha; ca = this.ca
    alpha = this.alpha}

}

class ExternalGearWheel extends BaseGearWheel{
//TODO проверка - допустимо ли такое число зубьев
}

class InternalGearWheel extends BaseGearWheel{
//TODO проверка - допустимо ли такое число зубьев
}

class Satellite extends BaseLink {
  //начальное количество колёс сателлита - 1
  val gear_wheels = new ArrayBuffer[BaseGearWheel](1)
  //прицепить к сателлиту еще колёсико
  def appendGearWheel(new_gear : BaseGearWheel) : Unit = gear_wheels.addOne(new_gear)
  //TODO eccentricity
}

class Carrier extends BaseLink {
  //TODO length
  def _eccentricity: Double = 0.0
  def _linkLength : Double
}

object StructureLib{
  val structures : Array[Array[BaseLink]] = Array(Array(new ExternalGearWheel{}))
}

