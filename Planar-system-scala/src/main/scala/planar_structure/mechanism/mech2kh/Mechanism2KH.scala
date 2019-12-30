package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.common_mechanisms.Constants
import planar_structure.mechanism.{Mechanism, MechanismFactory}
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition, External1, ExternalExternal, ExternalInternal, InternalInternal, MechanismType}
import planar_structure.mechanism.mech2kh.concrete_mechanisms._
import planar_structure.mechanism.process.argument.AdditionalWheelParams
import planar_structure.mechanism.process.report.{ Tension}
import planar_structure.subroutines.StandardParameters.MaterialTableRow


case class WheelInfo(z: Int,ca : Float = Constants.CA , ha: Float = Constants.HA, x: Float = Constants.X,
                     m : Float = Constants.M,
                     alpha: Float = Constants.ALPHA,
                     beta: Float = Constants.BETA,
                     materialTableRow: MaterialTableRow = null)

abstract class Mechanism2KH extends  Mechanism
//e.g. form of code: "ExternalExternal_CarrierInput"
object Mechanism2KH extends MechanismFactory {
  //надо бы вырезать это и задепрекейтить
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
      //--------------------------------
      case "External1" => new Mechanism2kh_E1(carrier)
    }
  }

  override def safeApply(code: String): Either[Boolean, Mechanism] = {
    try {
      Right(apply(code))
    }catch {
      case _ : Exception => Left(false)
    }
  }

  protected def setupMech(mechanism: Mechanism, additional: List[WheelInfo],  k: Byte) : Mechanism = {
    val gears = mechanism.getGears
    for (i <- Range(0, gears.length)){
      gears(i).holder.z = additional(i).z
      gears(i).holder.ca = additional(i).ca
      gears(i).holder.ha = additional(i).ha
      gears(i).holder.alpha = additional(i).alpha
      gears(i).holder.beta = additional(i).beta
      gears(i).holder.m = additional(i).m
      gears(i).holder.x = additional(i).x
      gears(i).material_holder.materialTableRow = additional(i).materialTableRow
    }
    mechanism.gearStructureCharacteristic.storage.mutable.amount_of_satellites = k
    mechanism
  }
  def apply(mechanismType: MechanismType, carrierPosition: CarrierPosition, wheelParams: List[WheelInfo],
            k: Byte): Mechanism = {
    val new_mech = mechanismType match {
      case ExternalExternal => {
        new Mechanism2kh_EE(carrierPosition)
      }
      //_----------------------------
      case ExternalInternal => new Mechanism2kh_EI(carrierPosition)
      //-------------------------------
      case InternalInternal => new Mechanism2kh_II(carrierPosition)
      //--------------------------------
      case External1 => new Mechanism2kh_E1(carrierPosition)

    }
    setupMech(new_mech, wheelParams, k)
  }

  def apply(mechanismType: MechanismType, carrierPosition: CarrierPosition, wheelParams: List[WheelInfo],
        k: Byte, wheelCalculator: WheelCalculator,
            allowedTension: Tension, realTension: Tension
           ): Mechanism = {
    val new_mech = apply(mechanismType, carrierPosition, wheelParams, k)
    val mech = setupMech(new_mech, wheelParams, k)
    mech.calculator = wheelCalculator
    mech.allowedTension = allowedTension
    mech.realTension = realTension
    mech
  }
}