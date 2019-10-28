package planar_structure.old.core_structure.connections

import planar_structure.old.core_structure.links.{AbstractHolder, ExternalWheelHolder, InternalWheelHolder, LinkElemHolder, WheelHolder}
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

sealed abstract class ConnectionHolder(first_ : LinkElemHolder, second_ : LinkElemHolder) extends AbstractHolder  with BeautifulDebugOutput {
  def first : LinkElemHolder = first_
  def second : LinkElemHolder = second_
  //initialize connection based on input
  def init() : Unit = {}
}

trait WheelConnectionCalculator extends GearObjectedConversions {
  this: GearConnectionHolder =>
  /*def updateAlpha_w()
  def findRw(alpha: Double, m: Double, z: Double): Double
  def updateAllRw()
  def updateAw()*/
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


sealed abstract class GearConnectionHolder(first_ : WheelHolder, second_ : WheelHolder)
  extends ConnectionHolder(first_, second_)
     with WheelConnectionCalculator {
  override def first: WheelHolder = first_
  override def second: WheelHolder = second_
  //инициализация
  init()
  override def init(): Unit = {
    //перерасчет alpha_w
    /*updateAlpha_w()
    //перерасчет всех Rw
    updateAllRw()
    //перерасчет Aw
    updateAw()*/
  }
  override def toStringFull: String = s"alpha_w: $alpha_tw\nrw1: $rw1\nrw2: $rw2\naw: $aw"
  override def toString: String = toStringShort
  override def toStringShort : String = s"connection of (${first.toStringShort}, ${second.toStringShort})"

}

class InternalConnectionHolder(first_ : WheelHolder, second_ : WheelHolder)
  extends GearConnectionHolder(first_, second_){

  super.init()
  //override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
 /* override def updateAlpha_w(): Unit = alpha_w ={
    (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  }
  override def updateAllRw(): Unit = {
    rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)
  }
  override def updateAw(): Unit = aw = rw1 + rw2*/
  def internal_external_packed : (ExternalWheelHolder, InternalWheelHolder) = {first match {
   case holder: InternalWheelHolder => ( second.asInstanceOf[ExternalWheelHolder],first.asInstanceOf[InternalWheelHolder])
   case _ => ( first.asInstanceOf[ExternalWheelHolder],second.asInstanceOf[InternalWheelHolder])
 }}
  override def toStringFull: String =  BeautifulDebugOutput.print("Internal connection:\n" +super.toString)
  override def toStringShort: String = "Internal " concat super.toStringShort
  def copy: InternalConnectionHolder = new InternalConnectionHolder(first, second)
  override def toString: String = toStringShort

  override def a: Double = {0.5*first.m * (internal_external_packed._2.z - internal_external_packed._1.z) / cos(first.beta)}
  override def x_d: Double = {val ext_int = internal_external_packed;
  ext_int._2.x - ext_int._1.x}
  override def d_w: Double = ???
  override def alpha_tw: Double = {
    val ext_int = internal_external_packed
    (2 * (ext_int._2.x - ext_int._1.x) * tan(alpha_t)/(ext_int._2.z - ext_int._1.z) + first.alpha_t.radToInv).invToRad
  }
  override def dw1: Double = {
    val ext_int = internal_external_packed
    ext_int._1.d + (2 * y / (ext_int._2.z - ext_int._1.z) * ext_int._1.d)
  }
  override def dw2 : Double = {
    val ext_int = internal_external_packed
    ext_int._2.d + (2 * y / (ext_int._2.z - ext_int._1.z) * ext_int._1.d)
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
  override def da1: Double = {
    get_da(first)
  }
  override def da2: Double = get_da(second)
}

class ExternalConnectionHolder(first : ExternalWheelHolder, second : ExternalWheelHolder)
  extends GearConnectionHolder(first, second){
  super.init()
 // override  def findRw(alpha : Double, m :Double, z: Double) : Double = m * z.toDouble / 2.0 * cos(alpha) / cos(alpha_w)
 /* override def updateAlpha_w(): Unit = alpha_w ={
    (first.alpha.radToInv + 2* (first.x + second.x) / (first.z + second.z).toDouble).invToRad
  }
  override def updateAllRw(): Unit = {
    rw1 = findRw(first.alpha, first.m, first.z); rw2 =  findRw(second.alpha, second.m, second.z)
  }
  override def updateAw(): Unit = aw = rw1 + rw2*/
  override def toStringFull: String =  BeautifulDebugOutput.print("External connection:\n" +super.toString)
  override def toStringShort: String = "External " concat super.toStringShort
  def copy: ExternalConnectionHolder = new ExternalConnectionHolder(first, second)
  override def toString: String = toStringShort

  override def a: Double = {0.5*first.m * (first.z + second.z) / cos(first.beta)}

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

