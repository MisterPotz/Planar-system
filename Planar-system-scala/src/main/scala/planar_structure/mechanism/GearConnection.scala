package planar_structure.mechanism
import planar_structure.help_traits.BeautifulDebugOutput
import planar_structure.mechanism.raw_algorithms.{ConnectionCalculation, ExternalConnectionCalculation, InternalConnectionCalculation}

import scala.math.{atan, cos, sin, tan}
//is used to calculate different useful things


abstract class WheelConnectionCommonCalculator(first : WheelHolder, second : WheelHolder) {
  def a : Float //делительное межосевое расстояние
  def alpha_t : Float = ConnectionCalculation.alpha_t(first) //приведенный угол alpha_t
  def aw: Float = ConnectionCalculation.aw(a,alpha_tw,first)//межосевое расстояние в зубчатой паре
  def alpha_tw : Float //угол зацепления приведенный - его мы должны через инолюту
  def x_d : Float //коэффициент разности смещений
  def y : Float = ConnectionCalculation.y(a, first,alpha_tw)//коэффициент воспринимаемого смещения
  def delta_y : Float = ConnectionCalculation.delta_y(x_d, a, first, alpha_tw) //коэффициент уравнительного смещений
  def d_w : Float //начальный диаметр
  def rw1 : Float = ConnectionCalculation.rw1(dw1)//начальный радиус первого колеса
  def rw2 : Float = ConnectionCalculation.rw2(dw2)//начальный радиус второго колеса
  def dw1 : Float
  def dw2 : Float
  def da1 : Float  //диаметр вершин зубьев первого колеса
  def da2 : Float //диаметр вершин зубьев второго колеса
  def stageSize : Float //размер ступени, для внтуреннего - 2*dсат + dсолн, для внутр - dвнутр
  def maxDw : Float = if (dw1 > dw2) dw1 else dw2
  def maxRw : Float = maxDw/2
  def get_da_by(holder : WheelHolder) : Float = {
    if (holder equals first){
      da1
    } else
      da2
  }
  def interference : Boolean //by default everything is ok
  def sign : Int
}

class InternalConnectionCalculationBehaviour(first : ExternalWheelHolder, second : InternalWheelHolder) extends WheelConnectionCommonCalculator(first, second) {
  override def a: Float = InternalConnectionCalculation.a(first, second)
  override def x_d: Float = InternalConnectionCalculation.x_d(first, second)
  override def d_w: Float = ???
  override def alpha_tw: Float = InternalConnectionCalculation.alpha_tw(first, second)
  override def dw1: Float = InternalConnectionCalculation.dw1(first, second, y)
  override def dw2 : Float =  InternalConnectionCalculation.dw2(first, second, y)
  override def da1: Float =  InternalConnectionCalculation.da1(first, second)
  override def da2: Float =  InternalConnectionCalculation.da2(first, second)
  override def interference: Boolean = InternalConnectionCalculation.interference(first, second)
  override def sign: Int = InternalConnectionCalculation.sign

  override def stageSize: Float = InternalConnectionCalculation.stageSize(first, second)
}

class ExternalConnectionCalculationBehaviour(first: ExternalWheelHolder, second : ExternalWheelHolder) extends WheelConnectionCommonCalculator(first, second){
  override def a: Float = ExternalConnectionCalculation.a(first, second)
  override def x_d: Float = ExternalConnectionCalculation.x_d(first, second)
  override def d_w: Float = ???
  override def alpha_tw: Float = ExternalConnectionCalculation.alpha_tw(first, second)
  override def dw1: Float = ExternalConnectionCalculation.dw1(first, second, y)
  override def dw2 : Float =  ExternalConnectionCalculation.dw2(first, second, y)
  override def da1: Float =  ExternalConnectionCalculation.da1(first, second)
  override def da2: Float =  ExternalConnectionCalculation.da2(first, second)
  override def interference: Boolean = ExternalConnectionCalculation.interference(first, second)
  override def sign: Int = ExternalConnectionCalculation.sign
  override def stageSize: Float = ExternalConnectionCalculation.stageSize(first, second)

}

class GearConnection(var gear1: GearWheel, var gear2 : GearWheel){
  var connectionType : ConnectionType = getType
  var connectionCalculationBehavior : WheelConnectionCommonCalculator =
    this.connectionType match {
      case (External) => new ExternalConnectionCalculationBehaviour(gear1.holder.asInstanceOf[ExternalWheelHolder]
        , gear2.holder.asInstanceOf[ExternalWheelHolder])
      case (Internal) => {
        val tuple = gear1.holder match {
          case a: ExternalWheelHolder => (a, gear2.holder.asInstanceOf[InternalWheelHolder])
          case a: InternalWheelHolder => (gear2.holder.asInstanceOf[ExternalWheelHolder], a)
        }
        new InternalConnectionCalculationBehaviour(tuple._1, tuple._2)
      }
    }
  def U : Float = gear2.holder.z.toFloat / gear1.holder.z.toFloat * connectionCalculationBehavior.sign
  def U_verse : Float = 1 / U
  sealed trait ConnectionType; case object Internal extends  ConnectionType; case object External extends ConnectionType;

  //initialize connection based on input

  def getType : ConnectionType = {
    (gear1, gear2) match {
      case (_ : ExternalWheel, _ : ExternalWheel) => External
      case (_ : ExternalWheel, _ : InternalWheel) => Internal
      case (_ : InternalWheel, _ : ExternalWheel) => Internal
    }
  }
}
