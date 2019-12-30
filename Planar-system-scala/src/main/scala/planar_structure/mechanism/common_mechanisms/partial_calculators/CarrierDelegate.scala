package planar_structure.mechanism.common_mechanisms.partial_calculators

import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, PartialWheelCalculator}
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition}

object CarrierDelegate {
  def apply(carrierPosition: CarrierPosition, calculator: PartialWheelCalculator) : CarrierDependent = {
    carrierPosition match {
      case CarrierInput => new CInDelegate(calculator)
      case CarrierOutput => new COutDelegate(calculator)
      case CarrierNeutral  => new CNeutralDelegate(calculator)
    }
  }
}
