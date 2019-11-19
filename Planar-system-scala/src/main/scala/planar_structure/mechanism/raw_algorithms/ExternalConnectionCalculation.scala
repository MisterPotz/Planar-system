package planar_structure.mechanism.raw_algorithms

import planar_structure.mechanism.GearGeometricCharacteristic
import math.{cos, Pi, sin, tan}

object ExternalConnectionCalculation extends ConnectionTypeDependentCalculations with GearObjectedConversions {
  override def a(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float =
    {0.5*first.m * (first.z.toFloat + second.z.toFloat) / cos(first.beta)}.toFloat
  override def alpha_tw(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float = {
    (2 * (second.x  + first.x) * tan(first.alpha_t)/(first.z.toFloat + second.z.toFloat) + first.alpha_t.radToInv).toFloat.invToRad
  }
  override def x_d(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float = first.x + second.x
  override def d_w: Float = ???
  override def dw1(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic, y : Float): Float = {
    first.d + (2 * y / (first.z.toFloat + second.z.toFloat) * first.d)  }
  override def dw2(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic, y : Float) : Float = {
    second.z + (2 * y / (first.z.toFloat +  second.z.toFloat) * first.d)  }
  override def da1(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float =
    first.d + 2 * (first.ha + first.x - ConnectionCalculation.delta_y(x_d(first, second),
      a(first, second),first, alpha_tw(first, second))) * first.m
  override def da2(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float =
    second.d + 2 * (second.ha + second.x - ConnectionCalculation.delta_y(x_d(first, second),
      a(first, second),first, alpha_tw(first, second))) * second.m
  override def interference(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Boolean = true
  override def sign: Int = -1
  override def stageSize(first: GearGeometricCharacteristic, second: GearGeometricCharacteristic): Float = {
    if (first.satelliteWheel){
      2*first.d + second.d
    } else {
      2 * second.d + first.d
    }//TODO не совсем понятно как понять, что здесь сателлит, а что - солнечное колесо
  }
}