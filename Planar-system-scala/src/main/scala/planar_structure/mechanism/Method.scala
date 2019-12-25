package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.methods.U
import planar_structure.mechanism.process.report.FullConditionCheck
import planar_structure.mechanism.report.file.{KinematicForwardReport, Report}
import planar_structure.mechanism.types.MechanismType

import scala.collection.immutable

abstract class Method



abstract class GeometricMethod extends Method with U  {
  var outerMethod : GeometricMethod = null
  var structure: GearStructureCharacteristic
  def setStructure(struct : GearStructureCharacteristic) = structure = struct
  def getGearRatio: Double //передаточное соотношение по умолчанию, то есть от входного звена к выходному
  def getGearRatioBackwards: Double // передаточное соотношение при другом направлении взгляда
  def getGearRatioCarrierStopped: Double // передаточное соотношение при остановленном водиле
  def alignmentCondition: Boolean //условие соосности
  def assemblyCondition: Boolean //условие сборки
  def interferenceCondition(i: Int): Boolean //условие интерференции (её отсутствия)
  def neighborhoodCondition: Boolean //условие соседства
  def noPruningOnGear(i: Int): Boolean //нет подрезания на колесе с данным индексом
  def noPruningOnAll: Boolean //нет подрезания на любом колесе
  // def overlapFactorOk : Boolean //коэффициент перекрытия на торце
  def minimalSize(a: List[GearStructureCharacteristic]): GearStructureCharacteristic
  def minimalSizeComparingTo(a: List[GearStructureCharacteristic]): Boolean //условие минимальных габаритов, сюда передается массив с другими страктами
  //и текущий набор сравнивается с другим, возвращается самый малый механизм
  def fullConditionCheck : FullConditionCheck
  def interferenceAll : Boolean
  //def getAW : Double
}

/**
 * this thing is used for kinematic analysis and synthesis
 */
abstract class KinematicMethod extends Method {
  /**
    gives inside information about the mechanism
   */
  def kinematicForwardAnalysis : KinematicForwardReport
 // def kinematicSynthesis(argument : KinematicSynthesisArgument) : Reporkt

}

abstract class MaterialStrengthMethod extends Method {
}



