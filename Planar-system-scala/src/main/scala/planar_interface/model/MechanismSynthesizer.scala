package planar_interface.model

import planar_structure.mechanism.common_mechanisms.CommonMechanismCharacteristics.{MechanismArgs, WheelNumberArgs}
import planar_structure.mechanism.common_mechanisms.{CommonMechanism, CommonMechanismCharacteristics, MECHANISM_FULL_CLASSIFIER, WheelCalculator}
import planar_structure.mechanism.process.report.SynthesizedMechanisms

object MechanismSynthesizer{

  def findMechanisms(args : MechanismArgs) : SynthesizedMechanisms = {
    val classi = CommonMechanismCharacteristics.pickSchemeByU(args.wheelNumberArgs.targetU)
    if (classi.isEmpty) throw new IllegalArgumentException("can't find defined mechanism")
    else classi.get.fullSynthesizer.mainScript(args)
    //classi.get.
  }
}
