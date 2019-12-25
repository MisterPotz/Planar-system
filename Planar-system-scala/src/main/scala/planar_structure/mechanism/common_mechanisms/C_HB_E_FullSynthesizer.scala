package planar_structure.mechanism.common_mechanisms

import planar_interface.model.MechanismSynthesizer
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.CommonMechanismCharacteristics.WheelNumberArgs
import planar_structure.mechanism.common_mechanisms.Common.{C_HB_E, CommonMechanismCharacteristics, FullSynthesizer}
import planar_structure.subroutines.{CylindricGearTransmissionsCalculation, StandardParameters}

import scala.collection.mutable.ListBuffer

object C_HB_E_FullSynthesizer extends FullSynthesizer(C_HB_E_WheelCalculator, C_HB_E) {

  override def getTorqueOn(wheelNumber: Int, wheelNumbers: List[Int], outputTorque: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => outputTorque * (wheelNumbers(0) * wheelNumbers(2)) /
        ((wheelNumbers(1) * wheelNumbers(3)))
      case 3 => outputTorque
      case 1 => getTorqueOn(0, wheelNumbers, outputTorque)
      case 2 => getTorqueOn(3, wheelNumbers, outputTorque)
    })
  }

  override def getFrequencyOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => 0
      case a if (a == 1 || a == 2) =>
        wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers) * (1 - wheelNumbers(0) / wheelNumbers(1))
      case 3 => wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers) *
        (wheelNumbers(3) * wheelNumbers(1) - wheelNumbers(0) * wheelNumbers(2)) / (wheelNumbers(3) * wheelNumbers(1))
    })
  }

  override def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => -wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case a if (a == 1 || a == 2) =>
        -wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers) * wheelNumbers(0) / wheelNumbers(1)
      case 3 => wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers) *
        wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers) *
        (wheelNumbers(3) * wheelNumbers(1) - wheelNumbers(0) * wheelNumbers(2)) / (wheelNumbers(3) * wheelNumbers(1) - 1)
    })
  }

  override def oneMechanismScript(gearNumbers: List[Int],
                                  material_pairs: ListBuffer[(StandardParameters.MaterialTableRow,
                                    StandardParameters.MaterialTableRow)], u: Double, accuracy: Double, satellites: Int,
                                  torque_output: Double, frequency_input: Double,
                                  save_buff: ListBuffer[Mechanism], cbeta2: Double): Unit = {
    oneMechanismScriptFull(gearNumbers, material_pairs, u, accuracy, satellites, torque_output, frequency_input, save_buff,
      cbeta2, wheelCalculator.getInners, wheelCalculator.getTargetRights)
  }
}


object C_HB_E_Test extends App {
  val aw1_corr = CylindricGearTransmissionsCalculation.findAWbyM(1.5, 124)
  val aw2_corr = CylindricGearTransmissionsCalculation.findAWbyM(2, 111)
  /*val kek = MechanismSynthesizer.findMechanisms(CommonMechanismCharacteristics.MechanismArgs(
    wheelNumberArgs = WheelNumberArgs(300, 0.05, 3),
    torqueOutput = 300,
    frequencyInput = 200
  ))*/
  for (i <- Range(40, 310, 10)){
    val some = MechanismSynthesizer.findMechanisms(CommonMechanismCharacteristics.MechanismArgs(
      wheelNumberArgs = WheelNumberArgs(i, 0.05, 3),
      torqueOutput = 300,
      frequencyInput = 200
    ))
    println(s"target u - ${i}: ${some.sorted_mechanisms.length}, satellites: ${some.sorted_mechanisms(0)
      .gearStructureCharacteristic.storage.k}")
  }

  /*val new_sime = some.sorted_mechanisms.filter(_.gearStructureCharacteristic.storage.gears.find(_.holder.beta != 0)
  match { case Some(_) => true;
    case None => false
  })
  println(new_sime)*/
}


