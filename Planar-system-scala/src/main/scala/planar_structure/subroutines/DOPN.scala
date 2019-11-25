package planar_structure.subroutines

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math._
import util.control.Breaks._

/**
 * if anyone reads this and wonders what kind of drugs the author had taken before
 * he wrote this code, you should know - this is NOT the whole authors idea with
 * an exceptionally high rate of matches and mutability.
 * the scripts down below are originally written in FORTRAN IV. The author had no
 * time to properly optimize it for functional
 * style so he followed the FORTRAN style as close as he could
 * for the sake of debugging. Sorry.
 */
object DOPN{
  val KHE_GLOBAL: Array[Float] = StandardParameters.KHE
  val KFE_GLOBAL: Array[Array[Float]] = StandardParameters.KFE

  def DOPN(N2 : Float, LH : Float, U : Float, NP : Float, NRR : Int, HRC1: Float, HRC2 : Float, SGT1 : Float,
           SGT2 : Float, SF : Float, NZAC1 : Float, NZAC2 : Float, ZETR : Float = ChangeableParameters.ZETR,
           YR : Float = ChangeableParameters.YR,
           V : Float =3f, M : Float=3f, KHE : Array[Float] = KHE_GLOBAL, KFE : Array[Array[Float]] = KFE_GLOBAL) :
  //SGHD,SGHMD, SGF1D, SGF2D, SGFM1D, SGFM2D
  (Float, Float, Float, Float, Float, Float) = {
    var current_loop_iteration : Int = 0
    //changes on continue operator
    var currentOperationNumber = 0
    def assignOperationNumber(operationToMatch : () => Float, on_low : Int, on_eq : Int, on_gt: Int) : Unit = {
      operationToMatch() match {
        case a if a < 0 =>
          currentOperationNumber = on_low
        case a if a  == 0 =>
          currentOperationNumber = on_eq
        case a if a > 0 =>
          currentOperationNumber = on_gt
      }
    }
    def assignOperationNumberN(some : Int) : Unit = currentOperationNumber=some
    val endNumber = 22

    var NF0 = 4.0e6f
    val N : mutable.ArrayBuffer[Float] = ArrayBuffer(N2*U, N2)
    val nhe2 = 60.0f * LH * N2 * KHE(NRR) * NZAC2
    val nhe1 = nhe2 * U * NZAC1 / NZAC2
    val NHE : mutable.ArrayBuffer[Float] = ArrayBuffer(nhe1, nhe2)
    val HRC : mutable.ArrayBuffer[Float] = ArrayBuffer(HRC1, HRC2)
    val SGT : mutable.ArrayBuffer[Float] = ArrayBuffer(SGT1, SGT2)
    val NZAC : mutable.ArrayBuffer[Float] = ArrayBuffer(NZAC1, NZAC2)

    val KFC1 : Float = StandardParameters.KFC(NRR)
    val KFC2 : Float = StandardParameters.KFC(NRR)
    val KFC : mutable.ArrayBuffer[Float] = ArrayBuffer(KFC1, KFC2)
    val SHD : mutable.ArrayBuffer[Float] = ArrayBuffer(0f, 0f)
    val NFE : mutable.ArrayBuffer[Float] = ArrayBuffer(0f, 0f)
    val SFD: mutable.ArrayBuffer[Float] = ArrayBuffer(0f, 0f)
    var SH = 0f;
    var SGH0 = 0f
    var SGF0 = 0f
    var SGHMD = 2.8f * SGT(0)
    val SFMD : mutable.ArrayBuffer[Float] = ArrayBuffer.fill(2){0.0f}
    var POKST = 1/6f
    var POKSTF = 1/6f
    var NH0 : Float = 0.0f
    var KHL : Float = 0.0f
    var KFL : Float = 0.0f
    var ZETV : Float = 1f
    var YSG : Float = 0f
    var SGH1D : Float = 0.0f; var SGF1D : Float = 0f; var SGFM1D : Float = 0f
    var SGH2D : Float = 0.0f; var SGF2D : Float = 0f; var SGFM2D : Float = 0f
    var SGHD = 0f
    while(currentOperationNumber != endNumber){
        currentOperationNumber match {
            //НАЧАЛО ЦИКЛА
          case 0 =>
            println("0")
            NH0 = (340f* pow(HRC(current_loop_iteration).toFloat, 3.146f) + 8.0e6).toFloat
            assignOperationNumber(() => HRC(current_loop_iteration) - 35f, 1, 1, 2)
          //ТЕРМООБРАБОТКА - УЛУЧШЕНИЕ
          case 1 =>
            println("1")
            SH=1.1f
            SGH0 = 20.0f*HRC(current_loop_iteration) + 70
            SGF0 = 18.0f * HRC(current_loop_iteration)
            SGHMD = 2.8f * SGT(current_loop_iteration)
            SFMD(current_loop_iteration) = 27.4f * HRC(current_loop_iteration)
            POKST = 1.0f / 6f
            POKSTF= 1f/6f
            if (KFC(current_loop_iteration) != 1) KFC(current_loop_iteration) = 0.65f
            assignOperationNumberN(5)
            //ТЕРМООБРАБОТКА: ЗАКАЛКА Т.В.Ч.
          case 2 =>
            println("2")
            SH = 1.2f
            SGH0 = 17.0f*HRC(current_loop_iteration) + 200
            SGF0 = 550
            SGHMD = 40.0f*HRC(current_loop_iteration)
            SFMD(current_loop_iteration) = 1430
            POKST = 1f/6f
            POKSTF = 1f/6f
            if (KFC(current_loop_iteration) != 1) KFC(current_loop_iteration) = 0.75f
            assignOperationNumber(() => HRC(current_loop_iteration) - 50.0f, 5, 3, 3)
            //ТЕРМООБРАБОТКА: ЦЕМЕНТАЦИЯ ИЛИ НИТРОЦЕМЕНТАЦИЯ
          case 3 =>
            println("3")
            SGH0 = 23f * HRC(current_loop_iteration)
            SGF0 = 850
            SFMD(current_loop_iteration) = 1450f
            POKST = 1f/6f
            POKSTF = 1f/9f
            if (KFC(current_loop_iteration) != 1) KFC(current_loop_iteration) = 0.9f
            assignOperationNumberN(5)
          case 5=>
            println("5")
            assignOperationNumber(() => (NHE(current_loop_iteration) - NH0).toFloat, 7, 7, 6)
          case 6 => println("6")
            NHE(current_loop_iteration) = NH0
            assignOperationNumberN(7)
          case 7 =>
            println("7")
            KHL = pow((NH0 / NHE(current_loop_iteration)), POKST).toFloat
            if (KHL >= 2.6f && HRC(current_loop_iteration) <= 35f) KHL = 2.6f
            if (KHL >= 1.8f && HRC(current_loop_iteration) > 35f) KHL = 1.8f
            ZETV = 1
            if (V > 5) ZETV = 0.85f*pow(V, 0.1).toFloat
            if (V > 5 && HRC(current_loop_iteration) > 35f) ZETV = 0.925f*pow(V, 0.05).toFloat
            SHD(current_loop_iteration) = SGH0 / SH * KHL * ZETR * ZETV
            NFE(current_loop_iteration) = 60f*LH *N(current_loop_iteration) * KFE(0)(NRR) * NZAC(current_loop_iteration)
            if (HRC(current_loop_iteration) > 35f)
              NFE(current_loop_iteration) = 60f * LH * N(current_loop_iteration)*KFE(1)(NRR)*NZAC(current_loop_iteration)
            assignOperationNumber(() =>(NFE(current_loop_iteration)-NF0).toFloat, 9,9,8)
          case 8 => println("8")
            NFE(current_loop_iteration) = NF0
            assignOperationNumberN(9)
          case 9 =>
            println("9")
            KFL = pow((NF0/NFE(current_loop_iteration)), POKSTF).toFloat
            if (KFL >= 2.08f && HRC(current_loop_iteration) <= 35f) KFL =2.08f
            if (KFL >= 1.63f && HRC(current_loop_iteration) > 35f) KFL =1.63f
            YSG = 1
            if (M != 3f)
              YSG =(1.18f - 0.1f*sqrt(M) + 0.006*M).toFloat
            SFD(current_loop_iteration) = SGF0 / SF *KFC(current_loop_iteration) * KFL * YSG * YR
            assignOperationNumber(() => current_loop_iteration - 1, 10,11,11)
          case 10 =>
            println("10")
            SGH1D = SHD(current_loop_iteration)
            SGF1D = SFD(current_loop_iteration)
            SGFM1D = SFMD(current_loop_iteration)
            assignOperationNumberN(12)
          case 11 =>
            println("11")
            SGH2D = SHD(current_loop_iteration)
            SGF2D = SFD(current_loop_iteration)
            SGFM2D = SFMD(current_loop_iteration)
            assignOperationNumberN(12)
          case 12 =>
            println(s"12 -- $current_loop_iteration")
            //lo
            if (current_loop_iteration < 1){
              current_loop_iteration=current_loop_iteration+1 //
              //возвращение в начало цикла с обновленным счетчиком
              assignOperationNumberN(0)
            }
            else{
              assignOperationNumberN(100)
            }
            //after exiting loop
          case 100 =>
            println("100")

            assignOperationNumber(() => NP - 3, 13,15,15)
          case 13 =>
            println("13")
            assignOperationNumber(() => HRC1 - HRC2 - 7f, 15,14,14)
          case 14 =>
            println("14")
            assignOperationNumber(() => HRC2 - 35f, 17,17,15)
          case 15 =>
            println("15")
            SGHD = SGH2D
            assignOperationNumber(() => SGH1D - SGHD, 16,22,22)
          case 16 =>
            println("16")
            SGHD = SGH1D
            assignOperationNumberN(22)
          case 17 =>
            println("17")
            SGHD = (0.45 * (SGH1D + SGH2D)).toFloat
            assignOperationNumber(() => NP -2, 18,20,20)
          case 18 =>
            println("18")
            assignOperationNumber(() =>(SGHD-1.23f*SGH2D).toFloat, 22,22,19)
          case 19 =>
            println("19")
            SGHD =1.23f*SGH2D
            assignOperationNumberN(22)
          case 20 =>
            println("20")
            assignOperationNumber(() => (SGHD-1.15*SGH2D).toFloat, 22,22,21)
          case 21 =>
            println("21")
            SGHD=1.15f*SGH2D
            assignOperationNumberN(22)
          case 22 | _  =>
            throw new IllegalStateException("Infinity loop in DOPN")
        }
    }
    //SGHD,SGHMD, SGF1D, SGF2D, SGFM1D, SGFM2D
    (SGHD, SGHMD, SGF1D, SGF2D, SGFM1D, SGFM2D)
  }


}
