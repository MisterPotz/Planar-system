package planar_structure.mechanism.common_mechanisms.Common

import planar_structure.mechanism.{InternalWheel, InternalWheelHolder, Mechanism}
import CommonMechanismCharacteristics.MechanismArgs
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.process.report.{AdditionalInfoSynthesized, SynthesizedMechanisms, Tension}
import planar_structure.mechanism.types.{CarrierOutput, ExternalInternal}
import planar_structure.subroutines.{ChangeableParameters, CylindricGearTransmissionsCalculation, SigF, StandardParameters}

import scala.collection.mutable.ListBuffer

abstract class FullSynthesizer(val wheelCalculator: WheelCalculator, val classi: MECHANISM_FULL_CLASSIFIER) {

  case class ShiftsBetas(shifts: List[Double], betas: List[Double])

  case class Compensations(m: Double, x: Double = 0, beta: Double = 0)

  def mainScript(mechanismArgs: CommonMechanismCharacteristics.MechanismArgs): SynthesizedMechanisms = {
    val u = mechanismArgs.wheelNumberArgs.targetU
    val accuracy = mechanismArgs.wheelNumberArgs.accuracy
    val satellites = mechanismArgs.wheelNumberArgs.satellites
    val torque_output = mechanismArgs.torqueOutput
    val frequency_input = mechanismArgs.frequencyInput
    //нашли тупые варики
    val dumb_variants = wheelCalculator.findInitialVariants(u, accuracy, satellites, gear_accuracy = 50)
    val filtered_dumb = wheelCalculator.neutralizeExtraVariants(dumb_variants)
    //определяем материалы колес и для первой и для второй ступеней, помним что первая - для мелкого, вторая - для большего
    val materials1 = StandardParameters.getMaterialsByProcessMode(5 /*mechanismArgs.processModes(0)*/)(0)
    val materials2 = materials1
    //немного подфильтровываем варики
    //val filtered = dumb_variants.zipWithIndex.filter(_._2 % 2 == 0)
    val mechanismsToSendBack = ListBuffer.empty[Mechanism]
    //теперь для каждого варика проходим весь цикл
    for (combination <- filtered_dumb.zipWithIndex) {
      oneMechanismScript(combination._1, ListBuffer(materials1, materials2), u, accuracy, satellites, torque_output, frequency_input,
        mechanismsToSendBack)
    }
    //теперь надо привести в порядок полученные значения
    val sorted_by_maxd = mechanismsToSendBack.filter(mech => {
      if (wheelCalculator.accurateAlignmentPercent(mech) <= accuracy * 100) {
        true
      } else false
    }).sortBy(mechanism => {
      getMaxDOfMech(mechanism)
    })
    println(s"size ${sorted_by_maxd}")
    val accuracy_sorted = sorted_by_maxd.map(mech => {
      wheelCalculator.accurateAlignmentPercent(mech)
    })
    val accuracy_U = sorted_by_maxd.map(mech => {
      val check = wheelCalculator.uCheckPercent(mech, u)
      println(s"u accuracy check : $check")
      check
    })
    val additionalInfoList = sorted_by_maxd.map(mech => {
      val _resU = mech.calculator.findU(mech)
      val _uAccuracy = mech.calculator.uCheckPercent(mech, u)
      val _aw = mech.calculator.awMech(mech, 1)
      val _awAccuracy = mech.calculator.accurateAlignmentPercent(mech)
      val _maximumSize = getMaxDOfMech(mech)
      val _allowedTension = mech.allowedTension
      val _realTension = mech.allowedTension
      AdditionalInfoSynthesized(resU = _resU, uAccuracy = _uAccuracy, alignmentAccuracy = _awAccuracy,
        aw2 = _aw, maximumSize = _maximumSize, allowedTension = _allowedTension, realTension = _realTension)
    })
    SynthesizedMechanisms(sorted_mechanisms = sorted_by_maxd, mechanismAmount = sorted_by_maxd.length,
      mechClassifier = classi, u_target = u, additionalInfo = additionalInfoList, minimalSize =
        getMaxDOfMech(sorted_by_maxd(0)))
  }

  def oneMechanismScript(gearNumbers: List[Int],
                         material_pairs: ListBuffer[(StandardParameters.MaterialTableRow,
                           StandardParameters.MaterialTableRow)],
                         u: Double,
                         accuracy: Double,
                         satellites: Int,
                         torque_output: Double,
                         frequency_input: Double,
                         save_buff: ListBuffer[Mechanism],
                         cbeta2: Double = 1.0
                        )

  def oneMechanismScriptFull(gearNumbers: List[Int],
                             material_pairs: ListBuffer[(StandardParameters.MaterialTableRow,
                               StandardParameters.MaterialTableRow)],
                             u: Double,
                             accuracy: Double,
                             satellites: Int,
                             torque_output: Double,
                             frequency_input: Double,
                             save_buff: ListBuffer[Mechanism],
                             cbeta2: Double = 1.0,
                             innerList_ : List[Boolean],
                             innerIsRightList: List[Boolean]
                            ): Unit = {
    val innerList = innerList_
    val t1 = getTorqueOn(0, gearNumbers, torque_output)
    val u1 = gearNumbers(1) / gearNumbers(0).toFloat
    val t2 = getTorqueOn(3, gearNumbers, torque_output)
    val n1 = getRelationalFreqOn(0, gearNumbers, frequency_input)
    val n2 = getRelationalFreqOn(1, gearNumbers, frequency_input)
    val n3 = getRelationalFreqOn(2, gearNumbers, frequency_input)
    val n4 = getRelationalFreqOn(3, gearNumbers, frequency_input)
    val u2 = gearNumbers(3) / gearNumbers(2).toFloat
    val pre_aw1 = CylindricGearTransmissionsCalculation.getPreliminaryAW(material_pairs(0)._1, material_pairs(0)._2, u1, t1, innerIsRightList(0))
    val pre_aw2 = CylindricGearTransmissionsCalculation.getPreliminaryAW(material_pairs(1)._1, material_pairs(1)._2, u2, t2, innerIsRightList(1))
    //println(s"pre_aw1: $pre_aw1, pre_aw2: $pre_aw2")

    def z_summ1: Int = wheelCalculator.z_sum1(gearNumbers)

    def z_summ2: Int = wheelCalculator.z_sum2(gearNumbers)

    val aw1 = CylindricGearTransmissionsCalculation.getAW(material_pairs(0)._1, material_pairs(0)._2, u1, t1, n1, n2, satellites, innerIsRightList(0))
    val aw2 = CylindricGearTransmissionsCalculation.getAW(material_pairs(1)._1, material_pairs(1)._2, u2, t2, n3, n4, satellites, innerIsRightList(1))
    val dsigh1 = CylindricGearTransmissionsCalculation.getDSigH(material_pairs(0)._1, material_pairs(0)._2, u1, t1, n1, n2, satellites, innerIsRightList(0))
    val dsigh2 = CylindricGearTransmissionsCalculation.getDSigH(material_pairs(1)._1, material_pairs(1)._2, u2, t2, n3, n4, satellites, innerIsRightList(1))
    val dsigh = math.min(dsigh1, dsigh2)
    val dsigf1 = SigF.getDSigF(material_pairs(0)._1, n1, 1)
    val dsigf2 = SigF.getDSigF(material_pairs(1)._2, n4, 1)
    val dsigf = math.min(dsigf1, dsigf2)
    val maxed_aw = math.max(aw1, aw2)
    //получаем целевое расстояние между ступенями, теперь нужно вычислить модуль
    //println(s"aw1: $aw1, aw2: $aw2")
    var m1 = CylindricGearTransmissionsCalculation.findM(maxed_aw, z_summ1)
    var m2 = CylindricGearTransmissionsCalculation.findM(maxed_aw, z_summ2)
    //нашли новое значение самого большого межосевого расстояния (зочем?)
    val findAWByM = CylindricGearTransmissionsCalculation.findAWbyM _
    val aw1_corr = findAWByM(m1, z_summ1)
    val aw2_corr = findAWByM(m2, z_summ2)
    val compensated_m = compensateM(gearNumbers, List(m1, m2), List(aw1_corr, aw2_corr), List(z_summ1, z_summ2))
    m1 = compensated_m._1
    m2 = compensated_m._2
    //после нахождения скомпенсированных модулей можно усреднить новые значения межосевых расстояний,
    // чтобы смещения были лучше распределены, это уже целевое межосевое расстояние
    val aw_corr = (findAWByM(compensated_m._1, z_summ1) + findAWByM(compensated_m._2, z_summ2)) / 2
    //после нахождения приближенных модулей и целевого межосевого расстояния,  надо рассчитать смещения
    val shiftsBetas: ListBuffer[ShiftsBetas] = compensateXBetAlpha(gearNumbers, List(m1, m2),
      aw_corr, List(z_summ1, z_summ2), cbeta2, math.abs(wheelCalculator.U_direct_H(u.toFloat)),
      u, accuracy, innerList)
    //после того как нашли шифнутые бетки, проверяем что они намутились и записываем если да
    shiftsBetas.foreach(value =>
      save_buff.addOne(Mechanism2KH(ExternalInternal, CarrierOutput, List(
        WheelInfo(z = gearNumbers(0), m = m1.toFloat, x = value.shifts(0).toFloat, beta = value.betas(0).toFloat, materialTableRow = material_pairs(0)._1),
        WheelInfo(z = gearNumbers(1), m = m1.toFloat, x = value.shifts(1).toFloat, beta = value.betas(0).toFloat, materialTableRow = material_pairs(0)._2),
        WheelInfo(z = gearNumbers(2), m = m2.toFloat, x = value.shifts(2).toFloat, beta = value.betas(1).toFloat, materialTableRow = material_pairs(1)._1),
        WheelInfo(z = gearNumbers(3), m = m2.toFloat, x = value.shifts(3).toFloat, beta = value.betas(1).toFloat, materialTableRow = material_pairs(1)._2)
      ),
        satellites.toByte, wheelCalculator, allowedTension = Tension(sigma_h = dsigh, dsigf),
        realTension = Tension(sigma_h = dsigh, dsigf)))
    )
    //TODO нужно ограничить выдаваемое число элементов,
    // можно проредить ряды до какого-то адекватногог количества (удалить допустим каждое значение с неким периодом
  }

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
  def getTorqueOn(wheelNumber: Int, wheelNumbers: List[Int], outputTorque: Double): Double

  def getFrequencyOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double

  import math.{pow, cos, sin, tan, atan}

  def calculateBeta1(wheelNumbers: List[Int], z_sum: List[Int], calpha: Double,
                     cbeta2: Double, m: List[Double], dirU: Double, aw: Double): Double = {
    val m_1 = m(0)
    val m_2 = m(1)
    val U_dir = dirU
    val a_w = aw
    val cosbeta_2 = cbeta2
    val z_1 = wheelNumbers(0)
    val z_2 = wheelNumbers(1)
    val z_3 = wheelNumbers(2)
    val z_4 = wheelNumbers(3)
    val z_sum1 = z_sum(0)
    val z_sum2 = z_sum(1)
    m_1 * z_1 * z_sum1 * (2 * U_dir * a_w * cosbeta_2 * z_3 + U_dir * cosbeta_2 * m_2 * z_3 * z_sum2 - U_dir * m_2 * z_3 * z_sum2
      - 2 * a_w * cosbeta_2 * z_3 - cosbeta_2 * m_2 * z_4 * z_sum2 + m_2 * z_3 * z_sum2) / (4 * U_dir * pow(a_w, 2) * cosbeta_2 * z_1 * z_3 +
      2 * U_dir * a_w * cosbeta_2 * m_1 * z_1 * z_3 * z_sum1 + 2 * U_dir * a_w * cosbeta_2 * m_2 * z_1 * z_3 * z_sum2
      - 2 * U_dir * a_w * m_2 * z_1 * z_3 * z_sum2 + U_dir * cosbeta_2 * m_1 * m_2 * z_1 * z_3 * z_sum1 * z_sum2
      - U_dir * m_1 * m_2 * z_1 * z_3 * z_sum1 * z_sum2 - 4 * pow(a_w, 2) * cosbeta_2 * z_1 * z_3
      - 2 * a_w * cosbeta_2 * m_1 * z_2 * z_3 * z_sum1 - 2 * a_w * cosbeta_2 * m_2 * z_1 * z_4 * z_sum2
      + 2 * a_w * m_2 * z_1 * z_3 * z_sum2 - cosbeta_2 * m_1 * m_2 * z_2 * z_4 * z_sum1 * z_sum2 + m_1 * m_2 * z_2 * z_3 * z_sum1 * z_sum2)
  }

  def calculateAlpha1(wheelNumbers: List[Int], z_sum: List[Int],
                      calpha: Double, cbeta2: Double, m: List[Double],
                      dirU: Double, aw: Double): Double = {
    val m_1 = m(0)
    val m_2 = m(1)
    val U_dir = dirU
    val a_w = aw
    val cosbeta_2 = cbeta2
    val z_1 = wheelNumbers(0)
    val z_2 = wheelNumbers(1)
    val z_3 = wheelNumbers(2)
    val z_4 = wheelNumbers(3)
    val z_sum1 = z_sum(0)
    val z_sum2 = z_sum(1)
    val cosalpha = cos(ChangeableParameters.ALF)
    cosalpha * (4 * U_dir * pow(a_w, 2) * cosbeta_2 * z_1 * z_3 +
      2 * U_dir * a_w * cosbeta_2 * m_1 * z_1 * z_3 * z_sum1 + 2 * U_dir * a_w * cosbeta_2 * m_2 * z_1 * z_3 * z_sum2
      - 2 * U_dir * a_w * m_2 * z_1 * z_3 * z_sum2 + U_dir * cosbeta_2 * m_1 * m_2 * z_1 * z_3 * z_sum1 * z_sum2
      - U_dir * m_1 * m_2 * z_1 * z_3 * z_sum1 * z_sum2 - 4 * pow(a_w, 2) * cosbeta_2 * z_1 * z_3 - 2 * a_w * cosbeta_2 * m_1 * z_2 * z_3 * z_sum1
      - 2 * a_w * cosbeta_2 * m_2 * z_1 * z_4 * z_sum2 + 2 * a_w * m_2 * z_1 * z_3 * z_sum2 - cosbeta_2 * m_1 * m_2 * z_2 * z_4 * z_sum1 * z_sum2
      + m_1 * m_2 * z_2 * z_3 * z_sum1 * z_sum2) / (2 * a_w * z_1 * (2 * U_dir * a_w * cosbeta_2 * z_3 + U_dir * cosbeta_2 * m_2 * z_3 * z_sum2
      - U_dir * m_2 * z_3 * z_sum2 - 2 * a_w * cosbeta_2 * z_3 - cosbeta_2 * m_2 * z_4 * z_sum2 + m_2 * z_3 * z_sum2))
  }

  def calculateAlpha2(wheelNumbers: List[Int], z_sum: List[Int], calpha: Double,
                      cbeta2: Double, m: List[Double], dirU: Double, aw: Double): Double = {
    val m_2 = m(1)
    val a_w = aw
    val cosbeta_2 = cbeta2
    val z_sum2 = z_sum(1)
    val cosalpha = cos(ChangeableParameters.ALF)
    cosalpha * m_2 * z_sum2 / (2 * a_w * cosbeta_2)
  }

  final def inv(angle: Double): Double = {
    math.tan(angle) - angle
  }

  def calculateOverallShift(alpha_w: Double, alpha_t: Double, z_sum: Int): Double = {
    (inv(alpha_w) - inv(alpha_t)) / 2 / math.tan(alpha_t) * z_sum
  }

  //итератор для удобного прохождения по доступным смещениям
  class ShiftIterator(totalShift: Double, inner: Boolean) extends Iterator[(Double, Double)] {
    var curr_first_shift: Double = if (totalShift < 0) StandardParameters.XS_N(0)
    else StandardParameters.XS(0) //StandardParameters.findNearestWithRound(StandardParameters.XS, totalShift)
    var counter = if (!inner) 0 else {
      val shift = StandardParameters.findNearestWithRound(
        if (totalShift < 0) {
          StandardParameters.XS_N
        } else StandardParameters.XS, totalShift)
      //после нахождения сдвига надо посмотреть его позицию
      val pos: Int = if (totalShift < 0) {
        StandardParameters.XS_N.indexOf(shift)
      } else StandardParameters.XS.indexOf(shift)
      pos
    }

    def valuesLeft: Int = StandardParameters.XS.length - counter

    def nextFirst: Double = {
      if (totalShift < 0)
        StandardParameters.XS_N(counter)
      else
        StandardParameters.XS(counter)
    }

    def recalculateSecond(first: Double): Double = {
      StandardParameters.findNearestWithRound(StandardParameters.XS, totalShift - first)
    }

    override def hasNext: Boolean = {
      //println(s"next passed with counter: ${counter}")
      if (counter >= StandardParameters.XS.length - 1)
        false
      else true
    }

    //Варьируем число
    /**
     *
     * @return возвращает смещения на первом колесе и на втором, причем вторым всегда является внутреннее колсе, в остальных
     *         случаях колеса считаются идущими по порядку
     */
    override def next(): (Double, Double) = {
      //println(s"curr counter: $counter")
      curr_first_shift = nextFirst
      counter += 1

      //если полное смещение отрицательное, то оно всё приходится на первое колеса  - второе внешнее
      if (inner && totalShift < 0) {
        (curr_first_shift, 0)
      } else
        (curr_first_shift, recalculateSecond(curr_first_shift))
    }
  }

  def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: List[Int], inputFrequency: Double): Double

  /**
   * нужна для поправки модуля, смещений и косинуса бета для зубьев для удовлетворения
   * передаточного отношения и условия соосности
   *
   * @param wheelNumbers
   * @param modules
   */
  def compensateM(wheelNumbers: List[Int], modules: List[Double],
                  aw: List[Double], z_summ: List[Int]): (Double, Double) = {
    //задать границу, после которой несоосность удовлетворяется смещениями, а не новым модулем
    /**
     * функция для нахождения аргумента, про котором данное выражение принимает минимальное значение
     * предполагается что в начальный
     *
     * @param func
     */
    def getMinArg(func: (Double) => Double, module: Double): Double = {
      val init_meaning = func(module)
      var new_meaning = init_meaning
      var old_meaning = init_meaning
      var saved_module = module
      var new_module = module
      do {
        new_module = StandardParameters.findNearest(StandardParameters.MS, saved_module + 0.01)
        new_meaning = func(new_module)
        if (new_meaning <= old_meaning) {
          old_meaning = new_meaning
          saved_module = new_module
        }
      } while (new_meaning <= old_meaning)
      saved_module
    }

    var aw_min = 0
    var aw_max = 0
    var m_start = 0.0
    if (aw(0) > aw(1)) {
      aw_min = 1
      aw_max = 0
      m_start = modules(1)
    } else if (aw(0) < aw(1)) {
      aw_max = 1
      aw_min = 0
      m_start = modules(0)
    } else {
      //если равны то уходим
      return (modules(0), modules(1))
    }

    val func: Double => Double = (some: Double) =>
      math.abs(CylindricGearTransmissionsCalculation.findAWbyM(some, z_summ(aw_min)) - aw(aw_max))

    val m_maxed = getMinArg(func, m_start)

    //println(s"${this.getClass.getName}: compensating m... $m_maxed, ${modules(0)} ----- ${modules(1)}")
    if (aw_min == 0) {
      (m_maxed, modules(1))
    } else
      (modules(0), m_maxed)
  }

  def compensateXBetAlpha(wheelNumbers: List[Int], modules: List[Double],
                          aw: Double, z_summ: List[Int], cbeta2: Double, dirU: Double, targetU: Double
                          , accuracyU: Double, inner: List[Boolean]):
  ListBuffer[this.ShiftsBetas] = {
    //сначала нужно определить по точному решению предположительные углы зацеплений и косозуб
    var calpha1 = calculateAlpha1(wheelNumbers, z_summ, math.cos(ChangeableParameters.ALF), cbeta2, modules, dirU, aw)
    if (calpha1 > 1) calpha1 = math.cos(25.toRadians)
    var cbeta1 = calculateBeta1(wheelNumbers, z_summ, math.cos(ChangeableParameters.ALF), cbeta2, modules, dirU, aw)
    var calpha2 = calculateAlpha2(wheelNumbers, z_summ, math.cos(ChangeableParameters.ALF), cbeta2, modules, dirU, aw)
    if (calpha2 > 1) calpha2 = math.cos(25.toRadians)
    //тепербь надо проверить что угол лежит в допустимых пределах
    var beta1 = (math.acos(cbeta1))
    val degreed_beta1 = beta1.toDegrees
    val beta2 = (math.acos(cbeta2))
    //проверяем бету первого колеса
    if (degreed_beta1 != 0) {
      if (degreed_beta1 < 7) {
        cbeta1 = 1
        beta1 = 0
      }
      else if (degreed_beta1 > 25) {
        cbeta1 = math.cos(math.toRadians(25))
        beta1 = (math.acos(cbeta1))
      }
    }
    var alpha1 = math.acos(calpha1)
    var alpha2 = math.acos(calpha2)
    //проверка углов зайеплений уже сложнее, так как надо сначала находить результирующее смещение
    //TODO здесь какое то сумасшедшее значение выходит
    var shift1 = calculateOverallShift(alpha1, ChangeableParameters.ALF, z_summ(0))
    if (math.abs(shift1) > 2) {
      if (shift1 < 0) {
        shift1 = -2
      }
      else shift1 = 2
    }

    val firstShiftIterator = new ShiftIterator(shift1, inner(0))

    var shift2 = calculateOverallShift(alpha2, (ChangeableParameters.ALF), z_summ(1))
    if (math.abs(shift2) > 2) {
      if (shift2 < 0) shift2 = -2
      else shift2 = 2
    }

    var secondShiftIterator = new ShiftIterator(shift2, inner(1))

    //нужно пересчитать новые значения передаточного отношения и соседства и определить их погрешность
    //если все херово то пересчитываем пока не закончатся варики в итераторе
    var successfulVarFound = false
    var currShift1 = (0.0, 0.0)
    var currShift2 = (0.0, 0.0)
    var z1_shift = 0.0
    val x1_min = wheelCalculator.xmin(wheelNumbers(0), beta1, alpha_t_ = wheelCalculator.alpha_t(beta = beta1.toFloat))
    var z2_shift = 0.0
    val x2_min = wheelCalculator.xmin(wheelNumbers(1), beta1, alpha_t_ = wheelCalculator.alpha_t(beta = beta1.toFloat))
    var z3_shift = 0.0
    val x3_min = wheelCalculator.xmin(wheelNumbers(2), beta2, alpha_t_ = wheelCalculator.alpha_t(beta = beta2.toFloat))
    var z4_shift = 0.0
    val x4_min = 0 //колесо внутреннее - подрезание минимальное это ноль
    var to_ret: ListBuffer[ShiftsBetas] = ListBuffer.empty[ShiftsBetas]
    var currentValuesLeft = firstShiftIterator.valuesLeft
    var shifts: List[Double] = null
    var betas: List[Double] = null
    var innerLoopCounter = 0
    //TODO lol онздесь заходит в бесконечный цикл потому что hasnext не меняется
    while (firstShiftIterator.hasNext && !successfulVarFound && currentValuesLeft > 0) {
      currentValuesLeft -= 1
      secondShiftIterator = new ShiftIterator(shift2, true)
      currShift1 = firstShiftIterator.next()
      //TODO вот здесь логика садится, так как итератор второй не следит за отчетом при внутреннем колесе
      innerLoopCounter = 0
      while (secondShiftIterator.hasNext && !successfulVarFound && innerLoopCounter < 2) {
        innerLoopCounter += 1
        //println(s"1 : ${firstShiftIterator.counter}\t2 : ${secondShiftIterator.counter}")
        //получили смещения
        currShift2 = secondShiftIterator.next()
        //проверка на подрезание
        z1_shift = currShift1._1
        z2_shift = currShift1._2
        z3_shift = currShift2._1
        z4_shift = currShift2._2

        //условия отсутствия подрезания
        if (z1_shift >= x1_min)
          if (z2_shift >= x2_min)
            if (z3_shift >= x3_min)
              if (z4_shift >= x4_min) {
                shifts = List(z1_shift, z2_shift, z3_shift, z4_shift)
                betas = List(beta1, beta2)
                //точность передаточного отношения с учетом смещений
                if (wheelCalculator.uCheck(wheelNumbers, shifts, betas,
                  targetU, accuracyU)) {
                  if (wheelCalculator.accurateAlignment(wheelNumbers,
                    shifts, List(beta1, beta2), modules, accuracyU)) {
                    to_ret.addOne(ShiftsBetas(shifts, betas))
                    if (to_ret.length >= 8) successfulVarFound = true
                  }
                }
              }
      }
    }
    if (to_ret.isEmpty) {
      return ListBuffer.empty
    }
    val u_good = to_ret.minBy(betas => {
      wheelCalculator.uCheckMeaning(wheelNumbers, betas.shifts, betas.betas, targetU, accuracyU)
    })
    val alignment_good = to_ret.minBy(betas => {
      wheelCalculator.accurateAlignmentPercent(wheelNumbers, betas.shifts, betas.betas, modules)
    })
    ListBuffer(u_good, alignment_good)
  }


  /**
   * только для двухрядных механизмов
   *
   * @param mechanism
   * @return
   */
  def getMaxDOfMech(mechanism: Mechanism): Double = {
    val gears = mechanism.getGears
    val z = gears.map(_.holder.z)
    val x = gears.map(_.holder.x)
    val z_sum1 = wheelCalculator.z_sum1(z)
    val z_sum2 = wheelCalculator.z_sum2(z)
    val x_sum1 = wheelCalculator.totalShift1(x.map(_.toDouble))
    val x_sum2 = wheelCalculator.totalShift2(x.map(_.toDouble))
    val beta1 = gears(0).holder.beta
    val beta2 = gears(3).holder.beta
    val m1 = gears(0).holder.m
    val m2 = gears(3).holder.m
    val inner1 = gears(0).holder.isInstanceOf[InternalWheelHolder]
    val innerr1 = false
    val inner2 = gears(3).holder.isInstanceOf[InternalWheelHolder];
    val innerr2 = true
    //сортируем по максимальному диаметру механизма
    wheelCalculator.findMaxDiameter(mechanism.getGears.map(_.holder.z), mechanism.getGears.map(_.holder.x),
      List(z_sum1, z_sum2), List(x_sum1, x_sum2), List(beta1, beta2), List(m1, m2), List(inner1, inner2), List(inner1, inner2))
  }
}