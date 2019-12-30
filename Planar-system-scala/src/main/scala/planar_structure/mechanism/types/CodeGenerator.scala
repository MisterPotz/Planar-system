package planar_structure.mechanism.types

trait CodeGenerator {
  implicit class Code(string: String){
    def toMechanismType : MechanismType = {
      string match {
        case "ExternalInternal" => ExternalInternal
       // case "InternalExternal" => InternalExternal
        case "ExternalExternal" => ExternalExternal
       // case "InternalInternal" => InternalExternal
        case "External1" => External1
        //case "Internal1" => Internal1
      }
    }
    def toCarrierType : CarrierPosition = {
      string match {
        case "CarrierInput" => CarrierInput
        case "CarrierOutput" => CarrierOutput
        case "CarrierNeutral" => CarrierNeutral
      }
    }
    def toFullType : (MechanismType, CarrierPosition) = {
      val a = string.split("_")
      (a(0).toMechanismType, a(1).toCarrierType)
    }
  }
}

object CodeGenerator{
  def apply(mechanismType: MechanismType, carrierPosition: CarrierPosition) : String ={
    mechanismType.toCode + "_" + carrierPosition.toCode
  }
}