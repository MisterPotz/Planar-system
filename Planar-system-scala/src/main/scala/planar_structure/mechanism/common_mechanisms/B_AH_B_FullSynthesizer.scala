package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.{CarrierOutput, ExternalInternal}
import planar_structure.subroutines.{CylindricGearTransmissionsCalculation, StandardParameters}

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
  override def mainScript(mechanismArgs: CommonMechanismCharacteristics.MechanismArgs): SynthesizedMechanisms = {
    val u = mechanismArgs.wheelNumberArgs.targetU
    val accuracy = mechanismArgs.wheelNumberArgs.accuracy
    val satellites = mechanismArgs.wheelNumberArgs.satellites
    val torque_output = mechanismArgs.torqueOutput
    val frequency_input = mechanismArgs.frequencyInput
    //нашли тупые варики
    val dumb_variants = wheelCalculator.findInitialVariants(u, accuracy, satellites, gear_accuracy = 2)
    //определяем материалы колес и для первой и для второй ступеней, помним что первая - для мелкого, вторая - для большего
    val materials1 = StandardParameters.getMaterialsByProcessMode(5 /*mechanismArgs.processModes(0)*/)(0)
    val materials2 = materials1
    //немного подфильтровываем варики
    val filtered = dumb_variants.zipWithIndex.filter(_._2 % 2 == 0)
    val mechanismsToSendBack = ListBuffer.empty[Mechanism]
    //теперь для каждого варика проходим весь цикл
    for (combination <- filtered) {
      val t1 = getTorqueOn(0, combination._1, torque_output)
      val u1 = combination._1(1) / combination._1(0).toFloat
      val t2 = getTorqueOn(3, combination._1, torque_output)
      val n1 = getRelationalFreqOn(0, combination._1, frequency_input)
      val n2 = getRelationalFreqOn(1, combination._1, frequency_input)
      val n3 = getRelationalFreqOn(2, combination._1, frequency_input)
      val n4 = getRelationalFreqOn(3, combination._1, frequency_input)
      val u2 = combination._1(3) / combination._1(2).toFloat
      val pre_aw1 = CylindricGearTransmissionsCalculation.getPreliminaryAW(materials1._1, materials1._2, u1, t1, false)
      val pre_aw2 = CylindricGearTransmissionsCalculation.getPreliminaryAW(materials1._1, materials1._2, u2, t2, true)
      println(s"pre_aw1: $pre_aw1, pre_aw2: $pre_aw2")

      def z_summ1: Int = combination._1(0) + combination._1(1)

      def z_summ2: Int = combination._1(3) - combination._1(2)

      val v1 = CylindricGearTransmissionsCalculation.getVelocity(materials1._1, materials1._2, u1, t1, n1, false)
      val v2 = CylindricGearTransmissionsCalculation.getVelocity(materials2._1, materials2._2, u2, t2, n3, true)
      val aw1 = CylindricGearTransmissionsCalculation.getAW(materials1._1, materials1._2, u1, t1, n1, n2, satellites, false)
      val aw2 = CylindricGearTransmissionsCalculation.getAW(materials2._1, materials2._2, u2, t2, n3, n4, satellites, false)
      val maxed_aw = math.max(aw1, aw2)
      println(s"aw1: $aw1, aw2: $aw2")
      val m1 = CylindricGearTransmissionsCalculation.findM(maxed_aw, z_summ1)
      val m2 = CylindricGearTransmissionsCalculation.findM(maxed_aw, z_summ2)
      println(s"m1: $m1, m2: $m2")
      mechanismsToSendBack.addOne(
        Mechanism2KH(ExternalInternal, CarrierOutput,
          List(
            WheelInfo(combination._1(0), m = m1.toFloat),
            WheelInfo(combination._1(1), m = m1.toFloat),
            WheelInfo(combination._1(2), m = m2.toFloat),
            WheelInfo(combination._1(3), m = m2.toFloat))
          , satellites.toByte))
    }
    SynthesizedMechanisms(mechanismsToSendBack, mechanismAmount = mechanismsToSendBack.length, -1, classi)

  }


  override def getTorqueOn(wheelNumber: Int, wheelNumbers: ListBuffer[Int], outputTorque: Double): Double = {
    wheelNumber match {
      case 0 => outputTorque * (wheelNumbers(0) * wheelNumbers(2)) /
        ((wheelNumbers(0) * wheelNumbers(2)) + (wheelNumbers(1) * wheelNumbers(3)))
      case 3 => outputTorque * (wheelNumbers(1) * wheelNumbers(3)) /
        ((wheelNumbers(0) * wheelNumbers(2)) + (wheelNumbers(1) * wheelNumbers(3)))
      case 1 => getTorqueOn(0, wheelNumbers, outputTorque)
      case 2 => getTorqueOn(3, wheelNumbers, outputTorque)
    }
  }

  override def getFrequencyOn(wheelNumber: Int, wheelNumbers: ListBuffer[Int], inputFrequency: Double): Double = {
    wheelNumber match {
      case 0 => inputFrequency
      case a if (a == 1 || a == 2) => inputFrequency * wheelNumbers(0) / wheelNumbers(1)
      case 3 => 0
    }
  }

  override def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: ListBuffer[Int], inputFrequency: Double): Double = {
    wheelNumber match {
      case 0 => inputFrequency - wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
      case a if (a == 1 || a == 2) => getRelationalFreqOn(0, wheelNumbers, inputFrequency) * wheelNumbers(0) / wheelNumbers(1)
      case 3 => -wheelCalculator.carrierFrequency(inputFrequency, wheelNumbers)
    }
  }

  /*//модули передабтся только от каждой ступени
  def compensateM(wheelNumbers: ListBuffer[Int], modules: List[Int],
                           aw: List[Double], z_summ: List[Int]): (Double, Double) = {
    //задать границу, после которой несоосность удовлетворяется смещениями, а не новым модулем
    /**
     * функция для нахождения аргумента, про котором данное выражение принимает минимальное значение
     * предполагается что в начальный
     * @param func
     */
    def getMinArg(func : (Double) => Double, module : Double) : Double = {
      val init_meaning = func(module)
      var new_meaning = init_meaning
      var saved_module = module
      var new_module = 0.0
      do {
        new_module = StandardParameters.findNearest(StandardParameters.MS, module+0.01)
        new_meaning = func(new_module)
        if (new_meaning <= init_meaning){
          saved_module = new_module
        }
      } while (new_meaning <= init_meaning)
      saved_module
    }
    var aw_min = 0
    var aw_max = 0
    var m_start = 0
    if (aw(0) > aw(1)) {
      aw_min = 1
      aw_max = 0
      m_start = modules(3)
    } else if (aw(0) < aw(1)) {
      aw_max = 1
      aw_min = 0
      m_start = modules(0)
    } else {
      //если равны то уходим
      return (modules(0), modules(1))
    }

    val func : Double => Double = (some : Double) =>
      math.abs(CylindricGearTransmissionsCalculation.findAWbyM(some, z_summ(aw_min)) - aw(aw_max))

    val m_maxed = getMinArg(func, m_start)

    println(s"${B_AH_B_FullSynthesizer.getClass.getName}: compensating m... $m_maxed")
    if (aw_min== 0){
      (m_maxed, modules(1))
    } else
      (modules(0), m_maxed)
  }
*/
//  def getCompensations(wheelNumbers: ListBuffer[Int], modules: List[Int],
//                                aw : List[Double], z_summ : List[Int]): ListBuffer[Compensations] = null
//
//  override protected def compensateM(wheelNumbers: ListBuffer[Int], modules: List[Int], aw: List[Double], z_summ: List[Double]): (Double, Double) = ???
}