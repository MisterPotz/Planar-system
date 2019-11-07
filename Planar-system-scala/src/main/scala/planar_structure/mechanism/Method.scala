package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.MechanismType

abstract class Method{

}
abstract class GeometricMethod extends Method{
  var methodable: GearStructureCharacteristic
  def getGearRatio : Double //передаточное соотношение по умолчанию, то есть от входного звена к выходному
  def getGearRatioBackwards : Double // передаточное соотношение при другом направлении взгляда
  def getGearRatioCarrierStopped : Double // передаточное соотношение при остановленном водиле
  def alignmentCondition : Boolean //условие соосности
  def assemblyCondition : Boolean //условие сборки
  def interferenceCondition(i : Int) : Boolean //условие интерференции (её отсутствия)
  def neighborhoodCondition : Boolean //условие соседства
  def noPruningOnGear(i : Int) : Boolean //нет подрезания на колесе с данным индексом
  def noPruningOnAll : Boolean //нет подрезания на любом колесе
 // def overlapFactorOk : Boolean //коэффициент перекрытия на торце
  def minimalSize(a : List[Characteristic]) : Characteristic
  def minimalSizeComparingTo(a : List[Characteristic]) : Boolean//условие минимальных габаритов, сюда передается массив с другими страктами
                                                  //и текущий набор сравнивается с другим, возвращается самый малый механизм
}


/**
 * used to describe:
 * 1) the entity on which the condition was used
 * 2) was the condition satisfied
 * 3) string which describes the condition and summarizes it
 */
trait Condition{

}
/**
 * defines an output for complex analysis and synthesis algorithms
 * 1) incapsulates various Condition objects for each used condition function
 */
trait Report
case class KinematicForwardReport(gearRatio : Float)
  extends Report
//TODO think on the hierarchy of mechanism class - methods - returning/input objects for methods and how they should be connected with controller/interface
case class KinematicSynthesisReport(gearRatio : Float)
  extends Report

/**
 * this thing is used for kinematic analysis and synthesis
 */
abstract class KinematicMethod extends Method {
  /**
    gives inside information about the mechanism
   */
  def kinematicForwardAnalysis : KinematicForwardReport
  def kinematicSynthesis(mechanismEssential: MechanismEssential) : Report

}

abstract class MaterialStrengthMethod extends Method {
}



