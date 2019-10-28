package planar_structure.mechanism

abstract class Method{
  var methodable : Methodable
  def setMethodable(methodable: Methodable) : Method = {
    this.methodable = methodable;
    this
  }
  def getMethodable : Methodable = methodable
}
class GeometricMethod extends Method{
  override var methodable: Methodable = _
}
class KinematicMethod extends Method {
  override var methodable: Methodable = _
}
class MaterialStrengthMethod extends Method {
  override var methodable: Methodable = _
}


//used be Method obbject as an argument
trait Methodable
trait GeometricMethodable extends Methodable
trait KinematicMethodable extends Methodable
trait MaterialStrengthMethodable extends Methodable



