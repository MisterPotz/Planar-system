package planar_structure.mechanism
import planar_structure.help_traits.BeautifulDebugOutput

import scala.math.{atan, cos, sin, tan}
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
trait WheelConnectionCommonCalculator extends GearObjectedConversions {
  /*def updateAlpha_w()
  def findRw(alpha: Double, m: Double, z: Double): Double
  def updateAllRw()
  def updateAw()*/
  var first : GearGeometricCharacteristic
  def a : Double //делительное межосевое расстояние
  def alpha_t : Double = first.alpha_t //приведенный угол alpha_t
  def aw: Double = a * cos(alpha_t) / cos(alpha_tw)//межосевое расстояние в зубчатой паре
  def alpha_tw : Double //угол зацепления приведенный - его мы должны через инолюту
  def x_d : Double //коэффициент разности смещений
  def y : Double = (aw - a)/first.m //коэффициент воспринимаемого смещения
  def delta_y : Double = x_d - y //коэффициент уравнительного смещений
  def d_w : Double //начальный диаметр
  def rw1 : Double = 0.5 * dw1 //начальный радиус первого колеса
  def rw2 : Double = 0.5 * dw2//начальный радиус второго колеса
  def dw1 : Double
  def dw2 : Double
  def da1 : Double  //диаметр вершин зубьев первого колеса
  def da2 : Double //диаметр вершин зубьев второго колеса
  def get_da_by(holder : WheelHolder) : Double = {
    if (holder equals first){
      da1
    } else
      da2
  }
}
trait ConnectionCalculationBehavior extends WheelConnectionCommonCalculator;
class InternalConnectionCalculationBehaviour(first_ : ExternalWheelHolder, second : InternalWheelHolder) extends ConnectionCalculationBehavior {
  def toStringFull: String =  BeautifulDebugOutput.print("Internal connection:\n" +super.toString)
  def toStringShort: String = "Internal connection"
  override def toString: String = toStringShort

  override var first: GearGeometricCharacteristic = first_
  override def a: Double = {0.5*first.m * (second.z - first.z) / cos(first.beta)}
  override def x_d: Double = second.x - first.x
  override def d_w: Double = ???
  override def alpha_tw: Double = {
    (2 * (second.x - first.x) * tan(alpha_t)/(second.z - first.z) + first.alpha_t.radToInv).invToRad
  }
  override def dw1: Double = {
    first.d + (2 * y / (second.z - first.z) * first.d)
  }
  override def dw2 : Double = {
    second.d + (2 * y / (second.z - first.z) * first.d)
  }
  protected def getWheel(holder : WheelHolder) : Either[InternalWheelHolder, ExternalWheelHolder] = {
    holder match {
      case a : InternalWheelHolder => Left(a)
      case a : ExternalWheelHolder => Right(a)
    }
  }
  protected def get_da(holder : WheelHolder) : Double = {
    getWheel(holder) match {
      case Left(a) => a.d - 2 * (a.ha - a.x - 0.2) * a.m
      case Right(a) => a.d + 2 * (a.ha + a.x) * a.m
    }
  }


  override def da1: Double = ???

  override def da2: Double = ???
}
class ExternalConnectionCalculationBehaviour(first_ : ExternalWheelHolder, second : ExternalWheelHolder) extends ConnectionCalculationBehavior{
  override def a: Double = {0.5*first.m * (first.z + second.z) / cos(first.beta)}

  override var first: GearGeometricCharacteristic = first_
  override def aw: Double = a * cos(alpha_t) / cos(alpha_tw)

  override def x_d: Double = first.x + second.x

  override def y: Double = (aw - a) / first.m
  override def d_w: Double = ???
  override def dw1: Double = first.d + (2 * y / (first.z + second.z) * first.d)
  override def dw2 : Double = second.z + (2 * y / (first.z + second.z) * first.d)
  override def alpha_tw: Double = {
    (2 * (second.x  + first.x) * tan(alpha_t)/(first.z + second.z) + first.alpha_t.radToInv).invToRad
  }

  override def da1: Double = first.d + 2 * (first.ha + first.x - delta_y) * first.m

  override def da2: Double = second.d + 2 * (second.ha + second.x - delta_y) * second.m
}
class GearConnection(var gear1: GearWheel, var gear2 : GearWheel){
  var connectionCalculationBehavior : ConnectionCalculationBehavior =
    connectionType match {
      case (External) => new ExternalConnectionCalculationBehaviour(gear1.holder.asInstanceOf[ExternalWheelHolder]
        , gear2.asInstanceOf[ExternalWheelHolder])
      case (Internal) => {
        val tuple = gear1 match {
          case a: ExternalWheelHolder => (a, gear2.asInstanceOf[InternalWheelHolder])
          case a: InternalWheelHolder => (gear2.asInstanceOf[ExternalWheelHolder], a)
        }
        new InternalConnectionCalculationBehaviour(tuple._1, tuple._2)
      }
    }
  var connectionType : ConnectionType = getType
  //initialize connection based on input
  sealed trait ConnectionType; case object Internal extends  ConnectionType; case object External extends ConnectionType;
  def getType : ConnectionType = {
    (gear1, gear2) match {
      case (_ : ExternalWheel, _ : ExternalWheel) => External
      case (_ : ExternalWheel, _ : InternalWheel) => Internal
      case (_ : InternalWheel, _ : ExternalWheel) => External
    }
  }
}
