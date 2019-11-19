package planar_structure.mechanism.types

import java.util.ResourceBundle

sealed trait ProcessType{
  def toCode : String
  val description : String
  override def toString: String = description
  protected val descriptionFileName : String = "MechanismNames"
}

case object KINEMATIC_ANALYSIS_FORWARD extends ProcessType{
  override def toCode: String = "KINEMATIC_ANALYSIS_FORWARD"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("process_kinematic_forward")

}
case object KINEMATIC_SYNTHESIS extends ProcessType{
  override def toCode: String = "KINEMATIC_SYNTHESIS"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("process_kinematic_synthesis")

}
case object STRENGTH_ANALYSIS_FORWARD extends ProcessType{
  override def toCode: String = "STRENGTH_ANALYSIS_FORWARD"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("process_strength_forward")

}
case object STRENGTH_SYNTHESIS extends ProcessType{
  override def toCode: String = "STRENGTH_SYNTHESIS"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("process_strength_synthesis")
}

object ProcessType{
  val KINEMATIC_ANALYSIS_FORWARD: String = "KINEMATIC_ANALYSIS_FORWARD"
  val KINEMATIC_SYNTHESIS : String = "KINEMATIC_SYNTHESIS"
  val STRENGTH_ANALYSIS_FORWARD : String = "STRENGTH_ANALYSIS_FORWARD"
  val STRENGTH_SYNTHESIS: String = "STRENGTH_SYNTHESIS"
}