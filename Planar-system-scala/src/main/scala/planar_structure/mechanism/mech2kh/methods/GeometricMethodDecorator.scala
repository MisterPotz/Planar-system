package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.{Characteristic, GearStructureCharacteristic, GeometricMethod}

//abstract decorator class for GeometricMethod, extends GeometricCommonMethod along with the GeometricMethod
abstract class GeometricMethodDecorator(decoratable : GeometricMethod) extends GeometricMethod {
  val decoratable_object : GeometricMethod = decoratable
  override var methodable: GearStructureCharacteristic = decoratable_object.methodable
  override def getGearRatio: Double = decoratable_object.getGearRatio
  override def getGearRatioBackwards: Double = decoratable_object.getGearRatioBackwards
  override def getGearRatioCarrierStopped: Double = decoratable_object.getGearRatioCarrierStopped
  override def alignmentCondition: Boolean = decoratable_object.alignmentCondition
  override def assemblyCondition: Boolean = decoratable_object.assemblyCondition
  override def interferenceCondition(i : Int): Boolean = decoratable_object.interferenceCondition(i)
  override def neighborhoodCondition: Boolean = decoratable_object.neighborhoodCondition
  override def noPruningOnGear(i: Int): Boolean = decoratable_object.noPruningOnGear(i)
  override def noPruningOnAll: Boolean = decoratable_object.noPruningOnAll
  //override def overlapFactorOk: Boolean = decoratable_object.overlapFactorOk
  override def minimalSize(a: List[Characteristic]): Characteristic = decoratable_object.minimalSize(a)
  override def minimalSizeComparingTo(a: List[Characteristic]): Boolean = decoratable_object.minimalSizeComparingTo(a)

}
