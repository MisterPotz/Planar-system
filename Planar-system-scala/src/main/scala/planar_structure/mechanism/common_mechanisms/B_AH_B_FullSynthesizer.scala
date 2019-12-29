package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.{B_AH_B, CommonMechanismCharacteristics, FullSynthesizer}
import planar_structure.mechanism.common_mechanisms.Common.CommonMechanismCharacteristics.WheelNumberArgs
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.process.report.{AdditionalInfoSynthesized, SynthesizedMechanisms, Tension}
import planar_structure.mechanism.types.{CarrierOutput, ExternalInternal}
import planar_structure.subroutines.{ChangeableParameters, CylindricGearTransmissionsCalculation, SigF, SigH, StandardParameters}

import scala.collection.mutable.ListBuffer

object B_AH_B_FullSynthesizer extends FullSynthesizer(B_AH_B_WheelCalculator, B_AH_B) {
  //def findMs(m1 : Double, m2 : Double, )
  /*
  сначала делаем "глупый" синтез по передаточному числу
  затем для выборки из предложенных вариантов делаем следующее:
    в главном скрипте - сначала определяются материалы колес
    затем определяется межосевое расстояние (пока будем отталкиваться от одного
    затем определяется модуль из этого расстояния (для каждой ступени свой, в зависимости от чисел  зубьев)
    затем производится компенсация с помозью смещений и косозубости
    затем вс> сортируется
    отдается результат
   */


  override def getTorqueOn(wheelNumber: Int, wheelNumbers: List[Int], outputTorque: Double): Double = {
    wheelNumber match {
      case 0 => outputTorque * (wheelNumbers(0) * wheelNumbers(2)) /
        ((wheelNumbers(0) * wheelNumbers(2)) + (wheelNumbers(1) * wheelNumbers(3)))
      case 3 => outputTorque * (wheelNumbers(1) * wheelNumbers(3)) /
        ((wheelNumbers(0) * wheelNumbers(2)) + (wheelNumbers(1) * wheelNumbers(3)))
      case 1 => getTorqueOn(0, wheelNumbers, outputTorque)
      case 2 => getTorqueOn(3, wheelNumbers, outputTorque)
    }
  }

  override def getFrequencyOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    wheelNumber match {
      case 0 => inputFrequency
      case a if (a == 1 || a == 2) => inputFrequency * wheelNumbers(0) / wheelNumbers(1)
      case 3 => 0
    }
  }

  override def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double = {
    wheelNumber match {
      case 0 => math.abs(inputFrequency - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers))
      case a if (a == 1 || a == 2) => math.abs(math.abs(getRelationalFreqOn(0, wheelNumbers, inputFrequency) * wheelNumbers(0) / wheelNumbers(1))
        - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers))
      case 3 => math.abs(-wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers))
    }
  }

  override def oneMechanismScript(gearNumbers: List[Int],
                                  material_pairs: ListBuffer[(StandardParameters.MaterialTableRow,
                                    StandardParameters.MaterialTableRow)], u: Double,
                                  accuracy: Double, satellites: Int, torque_output: Double,
                                  frequency_input: Double, save_buff: ListBuffer[Mechanism],
                                  cbeta2: Double): Unit = {
    oneMechanismScriptFull(gearNumbers,material_pairs,u, accuracy,satellites,torque_output,
      frequency_input,save_buff,cbeta2,wheelCalculator.getInners ,List(true,true))
  }
}

object B_AH_B_Test extends App {
  val aw1_corr = CylindricGearTransmissionsCalculation.findAWbyM(1.5, 124)
  val aw2_corr = CylindricGearTransmissionsCalculation.findAWbyM(2, 111)
  val compensated = B_AH_B_FullSynthesizer.compensateM(List(24, 100, 63, 174), List(1.5, 2), List(aw1_corr, aw2_corr), List(124, 111))
  val some = B_AH_B_FullSynthesizer.mainScript(CommonMechanismCharacteristics.MechanismArgs(
    wheelNumberArgs = WheelNumberArgs(12, 0.05, 3),
    torqueOutput = 300,
    frequencyInput = 200
  ))
  println(some.sorted_mechanisms.length)
  val new_sime = some.sorted_mechanisms.filter(_.gearStructureCharacteristic.storage.gears.find(_.holder.beta != 0) match { case Some(_) => true; case None => false })
  println(new_sime)

  /* val first = new B_AH_B_FullSynthesizer.ShiftIterator(0.4, false)
   while(first.hasNext){
     println(s"${first.next()} - first\t${first.counter}")
   }*/
}