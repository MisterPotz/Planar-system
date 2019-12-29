package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.CommonMechanismCharacteristics.WheelNumberArgs
import planar_structure.mechanism.common_mechanisms.Common.{A_AB_H, A_AH_B, A_CLASSIFIER, A_HB_A, CommonMechanismCharacteristics, FullSynthesizer}
import planar_structure.subroutines.StandardParameters

import scala.collection.mutable.ListBuffer

abstract class A_FullSynthesizer(calculator: A_WheelCalculator, clas: A_CLASSIFIER) extends FullSynthesizer(calculator, clas) {
  val carrierIsOutput: Boolean

  override def oneMechanismScript(gearNumbers: List[Int], material_pairs: ListBuffer[(StandardParameters.MaterialTableRow, StandardParameters.MaterialTableRow)],
                                  u: Double, accuracy: Double, satellites: Int, torque_output: Double,
                                  frequency_input: Double, save_buff: ListBuffer[Mechanism], cbeta2: Double): Unit = {
    oneMechanismScriptFull(gearNumbers, material_pairs, u, accuracy, satellites, torque_output,
      frequency_input, save_buff, cbeta2, wheelCalculator.getInners, List(true, true))
  }


  //считаем от момента на водиле, если же водило не является выходным звеном, нужно пересчитать момент на нем используя функции выше
  override def getTorqueOn(wheelNumber: Int, wheelNumbers: List[Int], outputTorque: Double): Double = {
    var carrierTorque: Double = math.abs({
      if (carrierIsOutput)
        outputTorque
      else {
        calculator match {
          case A_AB_H_WheelCalculator =>
            //выходным звеном считается колесо B
            val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
            outputTorque / (p / (p + 1))
          case A_HB_A_WheelCalculator =>
            //выходным звеном считается водило
            outputTorque
        }
      }
    })
    math.abs(wheelNumber match {
      case 0 => -carrierTorque / (math.abs(wheelCalculator.findUdH(wheelNumbers)) + 1)
      case 2 => -carrierTorque * math.abs(wheelCalculator.findUdH(wheelNumbers)) / (math.abs(wheelCalculator.findUdH(wheelNumbers)) + 1)
      case 1 => getTorqueOn(0, wheelNumbers, carrierTorque) - getTorqueOn(2, wheelNumbers, carrierTorque)
    })
  }
}

object A_AH_B_FullSynthesizer extends A_FullSynthesizer(A_AH_B_WheelCalculator, A_AH_B) {
  override val carrierIsOutput: Boolean = true

  override def getFrequencyOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => inputFrequency
      case 1 =>
        val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
        (1 + 2 * p / (1 - p)) * wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 2 =>
        0
    })
  }

  override def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => inputFrequency - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 1 =>
        val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
        (2 * p / (1 - p)) * wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 2 =>
        -wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
    })
  }
}

object A_AB_H_FullSynthesizer extends A_FullSynthesizer(A_AB_H_WheelCalculator, A_AB_H) {
  override val carrierIsOutput: Boolean = false

  override def getFrequencyOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => inputFrequency
      case 1 =>
        val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
        (1 + 2 * p / (1 - p)) * wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 2 =>
        inputFrequency / math.abs(wheelCalculator.findUdH(wheelNumbers))
    })
  }

  override def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => inputFrequency - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 1 =>
        val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
        (2 * p / (1 - p)) * wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 2 =>
        -wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
    })
  }
}

object A_HB_A_FullSynthesizer extends A_FullSynthesizer(A_HB_A_WheelCalculator, A_HB_A) {
  override val carrierIsOutput: Boolean = false

  override def getFrequencyOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => 0
      case 1 =>
        val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
        (1 + 2 / (p - 1)) * wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 2 =>
        val p = math.abs(wheelCalculator.findUdH(wheelNumbers))
        (1 + p) / (p) * wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
    })
  }

  override def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    math.abs(wheelNumber match {
      case 0 => inputFrequency - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 1 =>
        getFrequencyOn(1, wheelNumbers, inputFrequency) - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case 2 =>
        getFrequencyOn(2, wheelNumbers, inputFrequency) - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
    })
  }
}

object A_AH_B_Test extends App {
  val some = A_AH_B_FullSynthesizer.mainScript(CommonMechanismCharacteristics.MechanismArgs(
    wheelNumberArgs = WheelNumberArgs(5, 0.05, 3),
    torqueOutput = 300,
    frequencyInput = 200
  ))
  println(some.sorted_mechanisms.length)
  /*val new_sime = some.sorted_mechanisms.filter(_.gearStructureCharacteristic.storage.gears.find(_.holder.beta != 0) match { case Some(_) => true; case None => false })
  println(new_sime)*/
}