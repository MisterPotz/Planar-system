package planar_structure.core_structure.links

//contains some info about relative position of this link with respect to the previous link
abstract class LinkPositionHolder

//second axis higher than first is an additional variable to understand where the second wheel is located with respect to first in the scheme drawing
//also it is used to analyze relative locations of wheels and make the coaxiality condition work
//by default the previous link is lower than the current one
class WheelPositionHolder(val second_axis_higher_than_first : Boolean = true, val is_satellite_crown : Boolean = false) extends LinkPositionHolder{
}

object WheelPositionHolder{
  def default : WheelPositionHolder = higher
  def higher : WheelPositionHolder = new WheelPositionHolder(true)
  def lower : WheelPositionHolder = new WheelPositionHolder(false)
  def unapply(arg: WheelPositionHolder): Option[Boolean] = Some(arg.second_axis_higher_than_first)
}