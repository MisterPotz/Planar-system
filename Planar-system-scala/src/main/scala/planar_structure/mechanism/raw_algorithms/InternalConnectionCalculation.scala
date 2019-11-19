package planar_structure.mechanism.raw_algorithms
import planar_structure.mechanism.GearGeometricCharacteristic
import math.{cos, Pi, sin, tan}

object InternalConnectionCalculation extends ConnectionTypeDependentCalculations with GearObjectedConversions {
  override def a(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float = {0.5*first.m * (second.z - first.z) / cos(first.beta)}.toFloat
  override def alpha_tw(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float = {
    (2 * (second.x - first.x) * tan(first.alpha_t)/(second.z - first.z) + first.alpha_t.radToInv).toFloat.invToRad
  }
  override def x_d(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float = second.x - first.x
  override def d_w: Float = ???
  override def dw1(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic, y : Float): Float = {
    first.d + (2 * y / (second.z - first.z) * first.d)
  }
  override def dw2(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic, y : Float) : Float = {
    second.d + (2 * y / (second.z - first.z) * first.d)
  }
  override def da1(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float =  2 * first.r + 2 * (first.ha + first.x) * first.m
  override def da2(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Float =  (2 * second.r - 2 * (second.ha - second.x - 0.2) * second.m).toFloat
  override def interference(first : GearGeometricCharacteristic, second : GearGeometricCharacteristic): Boolean = {
    val z1 : Float = first.z
    val z2 : Float = second.z
    val alpha_a1 : Float = first.alpha_a
    val alpha_a2 : Float = second.alpha_a
    val a : Float = this.a(first, second)
    val alpha_tw : Float = this.alpha_tw(first, second)
    val aw : Float = ConnectionCalculation.aw(a, alpha_tw, first)
    val da1 : Float = this.da1(first, second)
    val da2: Float = this.da2(first, second)
    val gamma12 : Float = z1 / z2.toFloat * alpha_a1.radToInv - alpha_a2.radToInv + (1 - (z1 / z2.toFloat)) * alpha_tw.radToInv;
    val mu_max : Float = {
      val da2_pow = math.pow(da2, 2).toFloat
      val da1_pow = math.pow(da1, 2).toFloat
      val aw_pow = math.pow(aw, 2).toFloat
      val pre = ((da2_pow) - (da1_pow) - 4 * (aw_pow) / (4 * aw * da1))
      val pre_rad = math.acos(math.cos(pre)) / math.Pi
      math.acos(pre_rad).toFloat
    };
    val delta = {
      z1/z2 * mu_max - math.asin(da1 /da2 * sin(mu_max)) + gamma12
    }
    val eps_alpha : Float = ((z1 * tan(alpha_a1) - z2 * tan(alpha_a2) + (z2 - z1) * tan(alpha_tw)) / (2 * math.Pi)).toFloat
    val tmp : Float = if (first.beta == 0.0) 1.2f else 1.0f
    if (delta < 0 || eps_alpha < tmp) false else true
  }
  override def sign: Int = 1

  override def stageSize(first: GearGeometricCharacteristic, second: GearGeometricCharacteristic): Float = {
    second.d //делительный диаметр окружности колеса с внутренним зацеплением
  }
}