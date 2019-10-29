package planar_structure.mechanism

abstract class Method{
  var methodable : Methodable
  def setMethodable(methodable: Methodable) : Method = {
    this.methodable = methodable;
    this
  }
  def getMethodable : Methodable = methodable
}
abstract class GeometricMethod extends Method{
  override var methodable: Methodable
  def getGearRatio : Double
  def alignmentCondition : Boolean
  def assemblyCondition : Boolean
  def interferenceCondition : Boolean
  def neighborhoodCondition : Boolean
  def noPruningOnGear(i : Int) : Boolean
  def noPruningOnAll : Boolean
  def overlapFactorOk : Boolean
  def minimalSizeComparingTo(a : List[Methodable]) //условие минимальных габаритов, сюда передается массив с другими страктами
                                                  //и текущий набор сравнивается с другим. True - если текущий стракт самый малый
}
abstract class KinematicMethod extends Method {
  override var methodable: Methodable
}
abstract class MaterialStrengthMethod extends Method {
  override var methodable: Methodable = _
}


//used be Method obbject as an argument
trait Methodable
trait GeometricMethodable extends Methodable{
}
trait KinematicMethodable extends Methodable
trait MaterialStrengthMethodable extends Methodable



