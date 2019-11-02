package planar_structure.mechanism

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
abstract class KinematicMethod extends Method {

}
abstract class MaterialStrengthMethod extends Method {
}



