package planar_structure.mechanism.types

//used to understand what wheels are present in the mechanism
sealed trait MechanismType {
  def toCode : String
}

case object ExternalInternal extends MechanismType{

  override def toString: String = toCode
  override def toCode: String = "ExternalInternal"
}
case object InternalExternal extends MechanismType{
  override def toString: String = toCode
  override def toCode: String = "InternalExternal"

}
case object ExternalExternal extends MechanismType{
  override def toString: String = toCode
  override def toCode: String = "ExternalExternal"

}
case object InternalInternal extends MechanismType{
  override def toString: String = toCode
  override def toCode: String = "InternalInternal"

}
case object External1 extends MechanismType{ //простейший тип планетарного механизма
  override def toString: String = toCode
  override def toCode: String = "External1"
}
case object Internal1 extends MechanismType{ //такой же простой только наоборот
  override def toString: String = toCode
  override def toCode: String = "Internal1"

}
