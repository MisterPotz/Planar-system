package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.{Mechanism,  MechanismFactory}
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput}
import planar_structure.mechanism.mech2kh.concrete_mechanisms._

abstract class Mechanism2KH extends  Mechanism
//e.g. form of code: "ExternalExternal_CarrierInput"
object Mechanism2KH extends MechanismFactory {
  override def apply(code : String): Mechanism ={
    val carrier = code.split("_")(1) match {
      case "CarrierInput" => CarrierInput
      case "CarrierOutput" => CarrierOutput
      case "CarrierNeutral" => CarrierNeutral
    }
    code.split("_")(0) match {
      case "ExternalExternal" => new Mechanism2kh_EE(carrier)
      //_----------------------------
      case "ExternalInternal" => new Mechanism2kh_EI(carrier)
      //-------------------------------
      case "InternalInternal" => new Mechanism2kh_II(carrier)
      //-----------------------------------
      case "InternalExternal" => new Mechanism2kh_IE(carrier)
      //--------------------------------
      case "External1" => new Mechanism2kh_E1(carrier)
      //----------------------
      case "Internal1" => new Mechanism2kh_I1(carrier)
    }
  }

  override def safeApply(code: String): Either[Boolean, Mechanism] = {
    try {
      Right(apply(code))
    }catch {
      case _ : Exception => Left(false)
    }
  }
}