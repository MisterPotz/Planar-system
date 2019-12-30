package planar_structure.mechanism.common_mechanisms.partial_calculators

import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, PartialWheelCalculator}
import planar_structure.mechanism.types.{CarrierPosition, InternalInternal, MechanismType}

class IIPartialCalculator(carrierDelegate_ : CarrierPosition)  extends PartialWheelCalculator{
  override val mechanismType: MechanismType = InternalInternal

  override def sign: Double = 1

  override def getInners: List[Boolean] = List(true, false, false, true)

  override def getTargetRights: List[Boolean] = List(false, true)

  override def z_sum1(z: List[Int]): Int = z(1) - z(0)

  override def totalShift1(x: List[Double]): Double = x(1) - x(0)

  override def z_sum2(z: List[Int]): Int = z(3) - z(2)

  override def totalShift2(x: List[Double]): Double = x(3) - x(2)

  override def maxSupposedSatelliteGear(z: List[Int]): Double = math.max(z(1), z(2))

  override val carrierDelegate: CarrierDependent = CarrierDelegate(carrierDelegate_, this)
}