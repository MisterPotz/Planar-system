package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.GeometricMethod

class GeometricMethodCarrierInputDecorator(dec : GeometricMethod) extends GeometricMethodDecorator(dec) with U{
  override def getGearRatioBackwards: Double = {val res = U4h_1; println(s"CarrierInputDecorator backwards ratio: ${res}"); res}
  override def getGearRatio: Double = Uh4_1
}
