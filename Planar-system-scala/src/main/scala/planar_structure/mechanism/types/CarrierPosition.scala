package planar_structure.mechanism.types

import java.util.ResourceBundle

//position of carrier is important and is used to calculate proper gear ratio and understand structure of mechanism
sealed trait CarrierPosition{
  def toCode : String
  val description : String
  override def toString: String = description
  protected val descriptionFileName : String = "MechanismNames"
}
case object CarrierInput extends CarrierPosition {
  override def toCode: String = "CarrierInput"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("carr_input")

}
case object CarrierOutput extends CarrierPosition{
  override def toCode: String = "CarrierOutput"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("carr_output")

}

case object CarrierNeutral extends CarrierPosition{
  override def toCode: String = "CarrierNeutral"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("carr_neutral")
}
