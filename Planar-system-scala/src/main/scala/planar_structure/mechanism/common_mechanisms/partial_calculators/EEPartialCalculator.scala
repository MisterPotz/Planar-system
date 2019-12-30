package planar_structure.mechanism.common_mechanisms.partial_calculators

import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, PartialWheelCalculator}
import planar_structure.mechanism.types.{CarrierPosition, ExternalExternal, MechanismType}

class EEPartialCalculator(carrierDelegate_ : CarrierPosition) extends PartialWheelCalculator{
  override val mechanismType: MechanismType = ExternalExternal

  override def sign: Double = 1

  override def getInners: List[Boolean] = List(false, false, false, false)

  override def getTargetRights: List[Boolean] = List(true, false)

  override def z_sum1(z: List[Int]): Int = z(0) + z(1)

  override def totalShift1(x: List[Double]): Double = x(0) + x(1)

  override def z_sum2(z: List[Int]): Int = z(3) + z(2)

  override def totalShift2(x: List[Double]): Double = x(2) + x(3)

  override def maxSupposedSatelliteGear(z: List[Int]): Double = math.max(z(1), z(2))

  override val carrierDelegate: CarrierDependent = CarrierDelegate(carrierDelegate_, this)
}
