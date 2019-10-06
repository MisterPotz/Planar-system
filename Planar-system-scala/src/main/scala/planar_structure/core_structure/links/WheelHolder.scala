package planar_structure.core_structure.links

import planar_structure.core_structure.LinkSeq
import planar_structure.help_traits.BeautifulDebugOutput

import scala.collection.mutable

//used to create more advanced storages of case structures
abstract class LinkElemHolder


//these classes are used to hold info about the wheels. classes are used by higher structure
sealed abstract class WheelHolder(var z : Int,var  m: Double,var x:Double,var ha: Double = 1.0,var ca: Double =0.25,var alpha:Double=scala.math.toRadians(20.0)
,var  axis_steady: Boolean = true,var rotates : Boolean = true) extends LinkElemHolder with BeautifulDebugOutput {
  final def r : Double = m * z /2
  final def rb : Double = r * math.cos(alpha)
  override def toStringFull: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates\n"
  override def toStringShort : String = "gear wheel"
  override def toString: String = toStringShort
}

class InternalWheelHolder(z : Int = 30, m: Double = 1.25,x:Double = 0,
                          ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                          , axis_steady: Boolean = true, rotates : Boolean = true) extends WheelHolder(
  z, m,x,ha,ca,alpha, axis_steady, rotates) {
  override def toStringFull: String = BeautifulDebugOutput.print("Internal gear, parameters:\n"+ super.toStringFull)
  override def toStringShort : String = "Internal " + super.toStringShort
  override def toString: String = toStringShort
  def copy: InternalWheelHolder = new InternalWheelHolder(z,m,x,ha,ca,alpha,axis_steady,rotates)
}

class ExternalWheelHolder(z : Int = 30, m: Double = 1.25,x:Double = 0,
                          ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                          , axis_steady: Boolean = true, rotates : Boolean = true) extends WheelHolder(
  z, m,x,ha,ca,alpha, axis_steady, rotates) {
  override def toStringFull: String = BeautifulDebugOutput.print("External gear, parameters:\n"+ super.toStringFull)
  override def toStringShort : String = "External " + super.toStringShort
  def copy: InternalWheelHolder = new InternalWheelHolder(z,m,x,ha,ca,alpha,axis_steady,rotates)
  override def toString: String = toStringShort
}

object WheelHolder{
  def external : ExternalWheelHolder = new ExternalWheelHolder()
  def internal : InternalWheelHolder = new InternalWheelHolder()
}

class SatelliteHolder(val crowns : mutable.HashMap[Int, LinkSeq] = mutable.HashMap.empty[Int, LinkSeq],
                      satellitesAmount: Int = 3) extends LinkElemHolder

trait LinkHolderImplicits{
  implicit def hashMap2Holder(map : mutable.HashMap[Int, LinkSeq]) : SatelliteHolder = {
    new SatelliteHolder(map)
  }
}
