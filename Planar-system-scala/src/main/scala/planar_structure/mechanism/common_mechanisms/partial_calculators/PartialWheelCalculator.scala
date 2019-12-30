package planar_structure.mechanism.common_mechanisms.partial_calculators

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, PartialWheelCalculator}
import planar_structure.mechanism.types.{CarrierPosition, External1, ExternalExternal, ExternalInternal, InternalInternal, MechanismType}

object PartialWheelCalculator {
  def apply(mechanismType: MechanismType, carrierPosition: CarrierPosition): PartialWheelCalculator = {
    mechanismType match {
      case ExternalExternal =>
        new EEPartialCalculator(carrierPosition)
      case External1 =>
        new E1PartialCalculator(carrierPosition)
      case InternalInternal =>
        new IIPartialCalculator(carrierPosition)
      case ExternalInternal =>
        new EIPartialCalculator(carrierPosition)
    }
  }
}
