package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.GeometricMethod

class GeometricMethodCarrierNeutral(dec : GeometricMethod) extends GeometricMethodDecorator(dec) with U{
  override def getGearRatioBackwards: Double = U41_h
  override def getGearRatio: Double = U14_h
}