package planar_structure.mechanism.common_mechanisms.Common

import java.io.InputStream
import java.util.ResourceBundle

import planar_structure.mechanism.common_mechanisms._

sealed trait PlanarMechanismType

case object _2K_H extends PlanarMechanismType

case object _3K extends PlanarMechanismType


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
  override def U_direct_H(targetU: Float): Float = 1 - 1 / targetU

  override val wheelCalculator: WheelCalculator = C_HB_E_WheelCalculator
  override val fullSynthesizer: FullSynthesizer = C_HB_E_FullSynthesizer
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
                            CLASSIFIER: MECHANISM_FULL_CLASSIFIER,
                            allowedSatelliteAmount : Int = 9
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
    //CommonMechanism((3, 9), (0.97f, 0.99f), _2K_H, A_AH_B),
    //CommonMechanism((1.13f, 1.5f), (0.99f, 0.996f), _2K_H, A_HB_A),
    //CommonMechanism((-2, -8), (0.96f, 0.985f), _2K_H, A_AB_H),
    CommonMechanism((7, 16), (0.97f, 0.99f), _2K_H, B_AH_B),
    CommonMechanism((8, 300), (0.75f, 0.8f), _2K_H, C_HB_E)//,
    //CommonMechanism((25, 300), (0.4f, 0.9f), _2K_H, C_HB_E, allowedSatelliteAmount = 1)
  )

  def pickSchemeByU(u: Double): Option[MECHANISM_FULL_CLASSIFIER] = {
    COMMON_MECHANISMS.find(comm => {
      if (((comm.U._1) <= u && (comm.U._2) >= u) || (comm.U._2 <= u && comm.U._1 >= u)) {
        true
      } else
        false
    }).map(comm => comm.CLASSIFIER)
  }

  def getMaxSatelliteAmount(u : Double) : Option[Int] = {
    COMMON_MECHANISMS.find(comm => {
      if (((comm.U._1) <= u && (comm.U._2) >= u) || (comm.U._2 <= u && comm.U._1 >= u)) {
        true
      } else
        false
    }).map(comm => comm.allowedSatelliteAmount)
  }

}
