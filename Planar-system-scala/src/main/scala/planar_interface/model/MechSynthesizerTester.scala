package planar_interface.model

import java.util.function.DoubleToLongFunction

import planar_structure.mechanism.common_mechanisms.{B_AH_B, CommonMechanismCharacteristics, MECHANISM_FULL_CLASSIFIER}

/**
 * делегировать задачу расчета передаточ
 */
/*
object MechSynthesizerTester extends App{
  var some = B_AH_B.wheelCalculator.synthesizeByWheelNumber(CommonMechanismCharacteristics.WheelNumberArgs(10,0.05, 3))
  some = B_AH_B.wheelCalculator.sortedByMin(some)
  println(some.sorted_mechanisms.length)
  val s = some.sorted_mechanisms(0)
  println(s"${s.getGears.map{_.holder.z}}")
}*/
