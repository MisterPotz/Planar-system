package planar_structure.mechanism.raw_algorithms

import planar_structure.mechanism.GearGeometricCharacteristic

import scala.math.cos

object ConnectionCalculation{
  def alpha_t(first : GearGeometricCharacteristic) : Float = first.alpha_t //приведенный угол alpha_t
  def aw(a : Float, alpha_tw : Float, first : GearGeometricCharacteristic): Float = (a * cos(alpha_t(first)) / cos(alpha_tw)).toFloat//межосевое расстояние в зубчатой паре
  def y(a : Float, first : GearGeometricCharacteristic, alpha_tw: Float) : Float = (aw(a, alpha_tw,first) - a) /first.m //коэффициент воспринимаемого смещения
  def delta_y(x_d : Float,a : Float, first : GearGeometricCharacteristic, alpha_tw: Float) : Float = x_d - y(a, first, alpha_tw) //коэффициент уравнительного смещений
  def rw1(dw1 : Float) : Float = (0.5 * dw1).toFloat//начальный радиус первого колеса
  def rw2(dw2 : Float) : Float = (0.5 * dw2).toFloat//начальный радиус второго колеса
}

trait ConnectionTypeDependentCalculations{
  def a (first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float//делительное межосевое расстояние
  def alpha_tw(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float //угол зацепления приведенный - его мы должны через инолюту
  def x_d(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic) : Float //коэффициент разности смещений
  def d_w : Float //начальный диаметр
  def dw1(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic, y : Float) : Float
  def dw2(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic, y : Float) : Float
  def da1 (first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float  //диаметр вершин зубьев первого колеса
  def da2(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic) : Float //диаметр вершин зубьев второго колеса
  def interference(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic) : Boolean //by default everything is ok
  def sign : Int
  def stageSize(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic) : Float
}

trait GearObjectedConversions{
  implicit class InvToRad(i : Float) {
    self =>
    //подключение к классу Float дополнительных полезных функций
    //нахождение угла по инволюте обратной функцией инволюты по методу Ченга
    //TODO ограничить допустимый угол для использования функции ченга
    def invToRad: Float = (math.pow(3f * i, 1f / 3f) - (2 * i) / 5.0f + (9f / 175.0f) * math.pow(3f, 2f / 3.0f) *
      math.pow(i, 5f / 3.0f) - (2f / 175.0f) * math.pow(3, 1 / 3.0f) * math.pow(i, 7f / 3.0f) - (144f / 67375.0f) * math.pow(i, 3.0f) +
      (3258f / 3128125.0f) * math.pow(3, 2f / 3.0f) * math.pow(i, 11f / 3.0f) - (49711f / 153278125.0f) * math.pow(3, 1f/ 3.0f) *
      math.pow(i, 13f / 3.0f)).toFloat
    //нахождение инволюты по углу
    def radToInv: Float = (math.tan(i) - i).toFloat
  }
}
