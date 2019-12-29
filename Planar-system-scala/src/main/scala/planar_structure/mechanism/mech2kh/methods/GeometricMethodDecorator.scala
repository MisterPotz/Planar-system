package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.process.report.FullConditionCheck
import planar_structure.mechanism.{GearStructureCharacteristic, GeometricMethod}

//abstract decorator class for GeometricMethod, extends GeometricCommonMethod along with the GeometricMethod
abstract class GeometricMethodDecorator(decoratable : GeometricMethod) extends GeometricMethod {
  val decoratable_object : GeometricMethod = decoratable
  decoratable_object.outerMethod = this
  override var structure: GearStructureCharacteristic = decoratable_object.structure
  //must be overridden in subclasses
  override def getGearRatio: Double = outerMethod.getGearRatio
  //must be overridden in subclasses
  override def getGearRatioBackwards: Double = outerMethod.getGearRatioBackwards
  override def getGearRatioCarrierStopped: Double = decoratable_object.getGearRatioCarrierStopped
  //override def neighborhoodCondition: Boolean = decoratable_object.neighborhoodCondition

  override def U14_h: Double = getGearRatioCarrierStopped
}
