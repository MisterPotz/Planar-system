package planar_structure.mechanism.common_mechanisms

import java.io.InputStream
import java.util.ResourceBundle

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.A_AH_B.{PICTURE_ROOT, getClassificatorString}
import planar_structure.mechanism.common_mechanisms.A_HB_A.PICTURE_ROOT
import planar_structure.mechanism.common_mechanisms.CommonMechanismCharacteristics.{MechanismArgs, WheelNumberArgs}
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.CarrierOutput.descriptionFileName
import planar_structure.subroutines.{ChangeableParameters, CylindricGearTransmissionsCalculation, StandardParameters}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

sealed trait PlanarMechanismType

case object _2K_H extends PlanarMechanismType

case object _3K extends PlanarMechanismType


abstract class FullSynthesizer(val wheelCalculator: WheelCalculator, val classi : MECHANISM_FULL_CLASSIFIER) {
  case class Compensations(m : Double, x : Double = 0, beta : Double = 0)
  def mainScript(mechanismArgs: MechanismArgs): SynthesizedMechanisms

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
  def getTorqueOn(wheelNumber: Int, wheelNumbers: ListBuffer[Int], outputTorque: Double): Double

  def getFrequencyOn(wheelNumber: Int, wheelNumbers: ListBuffer[Int], inputFrequency: Double): Double
  def getRelationalFreqOn(wheelNumber: Int, wheelNumbers: ListBuffer[Int], inputFrequency: Double): Double

  /**
   * нужна для поправки модуля, смещений и косинуса бета для зубьев для удовлетворения
   * передаточного отношения и условия соосности
   * @param wheelNumbers
   * @param modules
   */
  /*def getCompensations(wheelNumbers : ListBuffer[Int], modules : List[Int],
                       aw : List[Double], z_summ : List[Double]) : ListBuffer[Compensations] = null //TODO

  protected def compensateM(wheelNumbers : ListBuffer[Int], modules : List[Int],
                            aw : List[Double], z_summ : List[Double]) : (Double, Double)*/

}

trait WheelCalculator {
  def carrierFrequency(inputFreq: Double, wheelList:ListBuffer[Int], kpd : Double = 1) : Double
  protected def arange(start: Double, end: Double, step: Double): ListBuffer[Double] = {
    var curr = start
    val arrayBuf = ListBuffer.empty[Double]
    while (curr <= end) {
      arrayBuf.addOne(curr)
      curr += step
    }
    arrayBuf
  }

  protected def linspace(start: Double, end: Double, parts: Int): ListBuffer[Double] = {
    val step = (end - start) / parts.toFloat
    arange(start, end, step)
  }

  protected def isNotSimpleNumber(i: Int): Boolean = {
    if (i % 3 == 0 || i % 5 == 0 || i % 7 == 0 || i % 9 == 0) {
      true
    } else false
  }

  protected def canHaveSatellites(wheelNumber: Int, satellites: Int = 3): Boolean = {
    if (wheelNumber % satellites == 0) true else false
  }

  case class ShiftedWheel(z: Int, shift: Double = 0)

  case class WithShiftedWheels(wheels: List[ShiftedWheel])

  def U_direct_H(targetU: Float): Float

  def findTargetU(u_directH: Double): Double

  //u check
  def uCheck(listBuffer: ListBuffer[Int], targetU: Double, accuracy: Double = 0.05): Boolean = {
    val currentU = findTargetU(findUdH(listBuffer))
    if (math.abs(targetU) * (1 - accuracy) <= math.abs(currentU) && math.abs(targetU) * (1 + accuracy) >= math.abs(currentU)) {
      true
    } else false
  }

  def findUdH(listBuffer: ListBuffer[Int]): Double

  //assembly
  def assemblyCheck(wheelNumbers: ListBuffer[Int], satellites: Int = 3): Boolean

  //unaccurate alignment
  def unaccurateAlignment(wheelNumbers: ListBuffer[Int], gears_accuracy: Int = 2): Boolean

  //neighborhood
  def neighborhoodCheck(wheelNumbers: ListBuffer[Int], satellites: Int): Boolean

  //hta
  def hta(ha: Float = ChangeableParameters.HA, beta: Float = ChangeableParameters.BETFS): Double = ha / math.cos(beta).toFloat

  //xt
  def xt(x: Double)(beta: Float = ChangeableParameters.BETFS): Double = x / math.cos(beta).toFloat

  //alpha_t
  def alpha_t(alpha: Float = ChangeableParameters.ALF, beta: Float = ChangeableParameters.BETFS): Double = math.atan(math.tan(alpha) / math.cos(beta))

  //pruning on separate wheel
  def pruning(z: Int, xt: Double, ha: Double, alpha_t: Double): Boolean = {
    val z_min = (2 * (ha - xt) / math.pow(math.sin(alpha_t), 2.0)).floor
    if (z_min > z) false else true
  }

  //min x to get rid of pruning
  def xmin(z: Int, beta: Double = ChangeableParameters.BETFS, ha: Double = ChangeableParameters.HA, alpha_t_ : Double = alpha_t()): Double = {
    ha - z * math.pow(math.sin(alpha_t_) , 2) / 2 / math.cos(beta)
  }

  //pruning for internal wheels
  def pruningForInternal(x: Double): Boolean = if (x < 0) true else false

  //some initial set of variants
  def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int = 2): ListBuffer[ListBuffer[Int]]

  //accurate alignment fix, must return paired z and x of both wheels. if no adequate combination is found - returns some message
  def accurateAlignment(z: IndexedSeq[Int])(alpha_t_ : Double = alpha_t()): List[ShiftedWheel]

  //final set of variants
  def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[WithShiftedWheels]

  /*//synthesize mechanisms
  def synthesizeByWheelNumber(wheelNumberArgs: WheelNumberArgs) : ListBuffer[WithShiftedWheels] = {
    val initial = findInitialVariants(targetU = wheelNumberArgs.targetU, accuracyU = wheelNumberArgs.accuracy, satellites = wheelNumberArgs.satellites)
    val final_varis = findFinalVariants(initial)
    val sorted_mechanism_list = sortedByMin(final_varis)
    //TODO SynthesizedMechanisms на этом уровне НЕ может использоваться, так как на данном этапе МОДУЛЬ НЕИЗВЕСТЕН
    sorted_mechanism_list
  }
  def minimalSize(wheels : List[ShiftedWheel]) : Int
  //sort synthesized mechanisms
  def sortedByMin(list: ListBuffer[WithShiftedWheels]) : ListBuffer[WithShiftedWheels] = {
    list.sortBy(mech => {
      minimalSize(mech.wheels)
    })
  }*/
}

trait MechanismCalculator {
  def synthesizeMechanisms()

  def findAllowedTensions()

}

trait TensionFinderTrait {

  case class HTension(DSigH: Double, SigHlim: Double, SH: Double, ZR: Double, ZV: Double, KHL: Double)

  case class FTension(DSigF: Double, SigFlim: Double, SF: Double, YR: Double, YS: Double, KFL: Double, KFC: Double)

  def findAllowedTensions(): (HTension, FTension)

  def findHTension(): HTension

  def findFTension(): FTension
}

sealed trait MECHANISM_FULL_CLASSIFIER {
  def U_direct_H(targetU: Float): Float

  val wheels: Int
  val wheelCalculator: WheelCalculator = null
  val fullSynthesizer: FullSynthesizer = null
  val stringClassificator: String
  val PICTURE_ROOT: String = "/mechanism_pictures/"
  val PICTURE_PATH: InputStream

  final protected def getClassificatorString(string: String): String = {
    ResourceBundle.getBundle("MechanismNames").getString(string)
  }

  val WHEEL_INDECES: List[String]
}

sealed trait A_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER {
  override val wheels: Int = 3
  override val WHEEL_INDECES = List("a", "g", "b")
}

object A_AH_B extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = {
    1 - targetU
  }
  //TODO
  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream(PICTURE_ROOT + "aahb.jpg")
  override val stringClassificator = getClassificatorString("aahb")
  override val wheelCalculator: WheelCalculator = A_AH_B_WheelCalculator
}

object A_AB_H extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = targetU

  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream(PICTURE_ROOT + "aaaabh.jpg")
  override val stringClassificator = getClassificatorString("aabh")
  override val wheelCalculator: WheelCalculator = A_AB_H_WheelCalculator

}

object A_HB_A extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 - targetU

  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream(PICTURE_ROOT + "ahba.jpg")
  override val stringClassificator = getClassificatorString("ahba")
  override val wheelCalculator: WheelCalculator = A_HB_A_WheelCalculator

}

sealed trait B_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER {
  override val wheels: Int = 4
}

object B_AH_B extends B_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 - targetU

  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream(PICTURE_ROOT + "bahb.jpg")
  override val wheelCalculator: WheelCalculator = B_AH_B_WheelCalculator
  override val fullSynthesizer: FullSynthesizer = B_AH_B_FullSynthesizer
  override val stringClassificator = getClassificatorString("bahb")
  override val WHEEL_INDECES = List("a", "g", "f", "b")
}

sealed trait C_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER {
  override val wheels: Int = 4
}

object C_HB_E extends C_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 / (1 - 1 / targetU)

  override val wheelCalculator: WheelCalculator = C_HB_E_WheelCalculator

  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream(PICTURE_ROOT + "chbe1.jpg")
  override val stringClassificator = getClassificatorString("chbe")
  override val WHEEL_INDECES = List("e", "f", "g", "b")
}


/**
 *
 * @param U          передаточное отношение механизма. т.к. типы механизмов разные
 *                   то здесь U принимается таким, каким оно обычно приводится в справочниках для того или иного типа механизма,
 *                   то есть может означать как п.о. при остановленном звене b, так и при остановленном звене а.
 * @param KPD        типичная для данного типа механизма область определения передаточного отношения
 * @param TYPE       тип планетарной передачи
 * @param CLASSIFIER классификатор по Кудрявцеву (табл. 1.1)
 */
case class CommonMechanism(
                            U: (Float, Float),
                            KPD: (Float, Float),
                            TYPE: PlanarMechanismType,
                            CLASSIFIER: MECHANISM_FULL_CLASSIFIER
                          )


object CommonMechanismCharacteristics {

  //arguments for calculating wheel z
  case class WheelNumberArgs( //mechanismType: MECHANISM_FULL_CLASSIFIER,
                              targetU: Double,
                              accuracy: Double,
                              satellites: Int,
                            )

  //arguments for calculating full mechanism
  case class MechanismArgs(
                            wheelNumberArgs: WheelNumberArgs,
                            torqueOutput: Double,
                            frequencyInput: Double,
                            //режимы обработки для ступеней
                            processModes: Array[Int] = Array(3)
                          )

  val COMMON_MECHANISMS = Array(
    CommonMechanism((3, 9), (0.97f, 0.99f), _2K_H, A_AH_B),
    CommonMechanism((1.13f, 1.5f), (0.99f, 0.996f), _2K_H, A_HB_A),
    CommonMechanism((-2, -8), (0.96f, 0.985f), _2K_H, A_AB_H),
    CommonMechanism((7, 16), (0.97f, 0.99f), _2K_H, B_AH_B),
    CommonMechanism((8, 30), (0.75f, 0.8f), _2K_H, C_HB_E),
    CommonMechanism((25, 300), (0.4f, 0.9f), _2K_H, C_HB_E)
  )

  def pickSchemeByU(u: Double): Option[MECHANISM_FULL_CLASSIFIER] = {
    COMMON_MECHANISMS.find(comm => {
      if (((comm.U._1) <= u && (comm.U._2) >= u) || (comm.U._2 <= u && comm.U._1 >= u)) {
        true
      } else
        false
    }).map(comm => comm.CLASSIFIER)
  }

}
