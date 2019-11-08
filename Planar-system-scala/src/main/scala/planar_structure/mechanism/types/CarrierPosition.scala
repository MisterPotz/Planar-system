package planar_structure.mechanism.types

//position of carrier is important and is used to calculate proper gear ratio and understand structure of mechanism
sealed trait CarrierPosition{
  def toCode : String
}
case object CarrierInput extends CarrierPosition {
  override def toCode: String = "CarrierInput"
  override def toString: String = "CarrierInput"
}
case object CarrierOutput extends CarrierPosition{
  override def toCode: String = "CarrierOutput"
  override def toString: String = "CarrierOutput"
}

case object CarrierNeutral extends CarrierPosition{
  override def toCode: String = "CarrierNeutral"
  override def toString: String = "CarrierNeutral"
}
