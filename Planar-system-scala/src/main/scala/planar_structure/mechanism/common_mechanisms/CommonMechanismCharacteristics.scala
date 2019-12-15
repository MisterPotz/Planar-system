package planar_structure.mechanism.common_mechanisms

import java.io.InputStream
import java.util.ResourceBundle

import planar_structure.mechanism.common_mechanisms.A_AH_B.{PICTURE_ROOT, getClassificatorString}
import planar_structure.mechanism.common_mechanisms.A_HB_A.PICTURE_ROOT
import planar_structure.mechanism.common_mechanisms.CommonMechanismCharacteristics.WheelNumberArgs
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.CarrierOutput.descriptionFileName

sealed trait PlanarMechanismType
case object _2K_H extends PlanarMechanismType
case object  _3K extends PlanarMechanismType




trait WheelCalculator{
  //synthesize mechanisms
  def synthesizeByWheelNumber(wheelNumberArgs: WheelNumberArgs) : SynthesizedMechanisms
  def minimalSize(synthesizedMechanisms: SynthesizedMechanisms) : SynthesizedMechanisms
  //sort synthesized mechanisms
  def sortedByMin(synthesizedMechanisms: SynthesizedMechanisms) : SynthesizedMechanisms

}

sealed trait MECHANISM_FULL_CLASSIFIER{
  def U_direct_H(targetU : Float) : Float
  val wheels : Int
  val wheelCalculator : WheelCalculator = null
  val stringClassificator : String
  val PICTURE_ROOT: String = "/mechanism_pictures/"
  val PICTURE_PATH : InputStream
  final protected def getClassificatorString(string : String) : String = {
    ResourceBundle.getBundle("MechanismNames").getString(string)
  }
}
sealed trait A_CLASSIFIER extends  MECHANISM_FULL_CLASSIFIER{
  override val wheels: Int = 3
}
object A_AH_B extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = {
    1 - targetU
  }
  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream( PICTURE_ROOT + "aahb.jpg")
  override val stringClassificator = getClassificatorString("aahb")
}
object A_AB_H extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = targetU
  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream( PICTURE_ROOT + "aaaabh.jpg")
  override val stringClassificator = getClassificatorString("aabh")
}
object A_HB_A extends  A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 / (1 - 1 / targetU)
  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream( PICTURE_ROOT + "ahba.jpg")
  override val stringClassificator = getClassificatorString("ahba")
}

sealed trait B_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER{
  override val wheels: Int = 4
}
object B_AH_B extends B_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 - targetU
  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream( PICTURE_ROOT + "bahb.jpg")
  override val wheelCalculator: WheelCalculator = B_AH_B_WheelCalculator
  override val stringClassificator = getClassificatorString("bahb")
}

sealed trait C_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER{
  override val wheels: Int = 4
}
object C_HB_E extends C_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 / (1 - 1 / targetU)
  override val PICTURE_PATH: InputStream = classOf[MECHANISM_FULL_CLASSIFIER].getResourceAsStream( PICTURE_ROOT + "chbe.jpg")
  override val stringClassificator = getClassificatorString("chbe")
}


/**
 *
 * @param U передаточное отношение механизма. т.к. типы механизмов разные
 *          то здесь U принимается таким, каким оно обычно приводится в справочниках для того или иного типа механизма,
 *          то есть может означать как п.о. при остановленном звене b, так и при остановленном звене а.
 * @param KPD типичная для данного типа механизма область определения передаточного отношения
 * @param TYPE тип планетарной передачи
 * @param CLASSIFIER классификатор по Кудрявцеву (табл. 1.1)
 */
case class CommonMechanism(
                          U: (Float, Float),
                          KPD : (Float, Float),
                          TYPE : PlanarMechanismType,
                          CLASSIFIER : MECHANISM_FULL_CLASSIFIER
                          )


object CommonMechanismCharacteristics {
  //arguments for calculating
  case class WheelNumberArgs(//mechanismType: MECHANISM_FULL_CLASSIFIER,
                             targetU : Double,
                             accuracy : Double,
                             satellites : Int,
                            )
  val COMMON_MECHANISMS = Array(
    CommonMechanism((3,9),(0.97f,0.99f),_2K_H,A_AH_B),
    CommonMechanism((1.13f,1.5f),(0.99f,0.996f),_2K_H, A_HB_A),
    CommonMechanism((-2,-8),(0.96f,0.985f),_2K_H, A_AB_H),
    CommonMechanism((7,16),(0.97f,0.99f),_2K_H, B_AH_B),
    CommonMechanism((8,30),(0.75f,0.8f),_2K_H, C_HB_E),
    CommonMechanism((25,300),(0.4f,0.9f),_2K_H, C_HB_E)
  )
  def pickSchemeByU(u : Double) : Option[MECHANISM_FULL_CLASSIFIER]  = {
    COMMON_MECHANISMS.find(comm => {
      if (math.abs(comm.U._1) <= u && math.abs(comm.U._2) >= u){
        true
      } else
        false
    }).map(comm => comm.CLASSIFIER)
  }
}
