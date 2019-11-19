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
  override def alignmentCondition: Boolean = decoratable_object.alignmentCondition
  override def assemblyCondition: Boolean = decoratable_object.assemblyCondition
  override def interferenceCondition(i : Int): Boolean = decoratable_object.interferenceCondition(i)
  override def neighborhoodCondition: Boolean = decoratable_object.neighborhoodCondition
  override def noPruningOnGear(i: Int): Boolean = decoratable_object.noPruningOnGear(i)
  override def noPruningOnAll: Boolean = decoratable_object.noPruningOnAll
  //override def overlapFactorOk: Boolean = decoratable_object.overlapFactorOk
  override def minimalSize(a: List[GearStructureCharacteristic]): GearStructureCharacteristic = decoratable_object.minimalSize(a)
  override def minimalSizeComparingTo(a: List[GearStructureCharacteristic]): Boolean = decoratable_object.minimalSizeComparingTo(a)
  override def interferenceAll: Boolean = decoratable_object.interferenceAll
  override def U14_h: Double = getGearRatioCarrierStopped

  override def fullConditionCheck: FullConditionCheck = decoratable_object.fullConditionCheck
}
