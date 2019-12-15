package planar_interface.model

import planar_structure.mechanism.common_mechanisms.CommonMechanismCharacteristics.WheelNumberArgs
import planar_structure.mechanism.common_mechanisms.{CommonMechanism, CommonMechanismCharacteristics, MECHANISM_FULL_CLASSIFIER, WheelCalculator}
import planar_structure.mechanism.process.report.SynthesizedMechanisms

object MechanismSynthesizer{

  def synthesizeByWheelNumber(wheelNumberArgs: WheelNumberArgs) : SynthesizedMechanisms = {
    val classi = CommonMechanismCharacteristics.pickSchemeByU(wheelNumberArgs.targetU)
    if (classi.isEmpty) throw new IllegalArgumentException("can't find defined mechanism")
    classi.get.wheelCalculator.synthesizeByWheelNumber(wheelNumberArgs)
  }
}
