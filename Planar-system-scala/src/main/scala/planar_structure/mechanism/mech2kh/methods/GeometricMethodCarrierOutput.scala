package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.GeometricMethod

class GeometricMethodCarrierOutput(dec : GeometricMethod) extends GeometricMethodDecorator(dec) with U{
  override def getGearRatioBackwards: Double = Uh1_4 //TODO уточнить функции
  override def getGearRatio: Double = U1h_4
}