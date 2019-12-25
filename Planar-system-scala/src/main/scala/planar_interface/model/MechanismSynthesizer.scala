package planar_interface.model

import planar_structure.mechanism.common_mechanisms.Common.CommonMechanismCharacteristics
import planar_structure.mechanism.common_mechanisms.Common.CommonMechanismCharacteristics.{MechanismArgs, WheelNumberArgs}
import planar_structure.mechanism.process.report.SynthesizedMechanisms

object MechanismSynthesizer{

  def findMechanisms(args : MechanismArgs) : SynthesizedMechanisms = {
    val classi = CommonMechanismCharacteristics.pickSchemeByU(args.wheelNumberArgs.targetU)
    val satteliteAmount =  CommonMechanismCharacteristics.getMaxSatelliteAmount(args.wheelNumberArgs.targetU)
    if (classi.isEmpty) throw new IllegalArgumentException("Тип механизма с целевым U пока отсутствует в базе данных")
    else classi.get.fullSynthesizer.mainScript(args.copy(wheelNumberArgs = args.wheelNumberArgs.copy(
      //satellites = if (args.wheelNumberArgs.satellites > satteliteAmount.get) satteliteAmount.get else args.wheelNumberArgs.satellites)))
      satellites = 3 )))
  }
}
