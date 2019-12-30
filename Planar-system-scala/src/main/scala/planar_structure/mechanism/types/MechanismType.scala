package planar_structure.mechanism.types

import java.util.ResourceBundle

import planar_structure.mechanism.types.ExternalInternal.descriptionFileName

//used to understand what wheels are present in the mechanism
sealed trait MechanismType {
  def wheelsInGroup : Array[Byte]
  def groupsAmount : Byte
  def wheelsAmount : Byte
  def toCode : String
  val description : String
  override def toString: String = description
  protected val descriptionFileName : String = "MechanismNames"
}

case object ExternalInternal extends MechanismType{
 // def toStringFull : String = ResourceBundle.getBundle("MechanismNames").getString("mech_ii")
  override def toCode: String = "ExternalInternal"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("mech_ei")
  override def wheelsAmount: Byte = 4
  override def groupsAmount: Byte = 2
  override def wheelsInGroup: Array[Byte] = Array(2,2)
}
/*case object InternalExternal extends MechanismType{
  override def toCode: String = "InternalExternal"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("mech_ie")

  override def wheelsAmount: Byte = 4
  override def groupsAmount: Byte = 2
  override def wheelsInGroup: Array[Byte] = Array(2,2)


}*/
case object ExternalExternal extends MechanismType{
  override def toCode: String = "ExternalExternal"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("mech_ee")
  override def groupsAmount: Byte = 2
  override def wheelsInGroup: Array[Byte] = Array(2,2)
  override def wheelsAmount: Byte = 4
}
case object InternalInternal extends MechanismType{
  override def toCode: String = "InternalInternal"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("mech_ii")
  override def groupsAmount: Byte = 2
  override def wheelsInGroup: Array[Byte] = Array(2,2)

  override def wheelsAmount: Byte = 4
}
case object External1 extends MechanismType{ //простейший тип планетарного механизма
  override def toCode: String = "External1"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("mech_e1")
  override def groupsAmount: Byte = 1
  override def wheelsAmount: Byte = 3
  override def wheelsInGroup: Array[Byte] = Array(3)
}
/*case object Internal1 extends MechanismType{ //такой же простой только наоборот
  override def toCode: String = "Internal1"
  override lazy val description: String = ResourceBundle.getBundle(descriptionFileName).getString("mech_i1")
  override def groupsAmount: Byte = 1
  override def wheelsAmount: Byte = 3
  override def wheelsInGroup: Array[Byte] = Array(3)
}*/
