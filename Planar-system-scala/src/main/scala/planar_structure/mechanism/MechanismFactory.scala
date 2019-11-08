package planar_structure.mechanism

trait MechanismFactory{
  def apply(code : String) : Mechanism
  def safeApply(code : String) : Either[Boolean, Mechanism]
}