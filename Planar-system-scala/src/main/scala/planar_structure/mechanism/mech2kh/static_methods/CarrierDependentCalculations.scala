package planar_structure.mechanism.mech2kh.static_methods

import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition, External1, ExternalExternal, ExternalInternal, Internal1, InternalExternal, InternalInternal, MechanismType}

trait CarrierDependentCalculationsInterface {
  def Uh4_1 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 / U4h_1(z_list,mechanismType)
  def U4h_1 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 - U41_h(z_list,mechanismType)
  def U41_h (z_list : List[Short],mechanismType : MechanismType): Float =
    1 / U14_h(z_list,mechanismType)
  def U1h_4 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 - U14_h(z_list,mechanismType)
  def Uh1_4 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 /  U1h_4(z_list,mechanismType)
  def U14_h(z_list : List[Short],mechanismType : MechanismType): Float = {
    //TODO ASAP не учитывает тип зацепления
    //TODO  да он вообще не пойми как считает всё
    var modifier : Byte = 1
    mechanismType match {
      case ExternalExternal => modifier = 1
      case ExternalInternal => modifier = -1
      case InternalInternal => modifier = 1
      case InternalExternal => modifier = -1
      case External1 => modifier = -1
      case Internal1 => modifier = -1
    }
    z_list.length match {
      case 4 => {
        modifier * (z_list(1) * z_list(3)) / (z_list(0) * z_list(2)).toFloat
      }
      case 3 => {
        modifier * (z_list(2) / z_list(0).toFloat)
      }
    }
  }
  def calculateGearRatio(z_list: List[Short], carrierPos : CarrierPosition, mechanismType : MechanismType) : Float = {
    carrierPos match {
      case CarrierInput => Uh4_1(z_list, mechanismType)
      case CarrierOutput=> U1h_4(z_list,mechanismType)
      case CarrierNeutral => U14_h(z_list,mechanismType)
  }
  }
}

object CarrierDependentCalculations extends CarrierDependentCalculationsInterface
