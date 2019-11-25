package planar_structure.subroutines

import scala.math.{Pi, abs, atan, cos, pow, sin, sqrt}
import scala.util.control.Breaks.{break, breakable}

object ZUCF {

  def ZUCF(Z1 : Int, Z2 : Int, X1 : Float, X2 : Float, HRC1 : Float, HRC2 : Float,
           AW : Float, M: Float, BW1 : Float, BW2 : Float, U : Float, SIGN : Int, FT : Float,
           TTED : Float, V : Float, PSIBD : Float, YEP : Float, NWR : Int,
          //---------------------
            HG: Float = ChangeableParameters.HG, BET : Float = ChangeableParameters.BETFS, EPALF : Float, EPBET : Float, Z0: Float, G0: Float, ST : Int,
           WV : Float, CONSOL : Int, KFB : Float) :
  //SGF1, SGF2, SGFM1, SGFM2, KFV, KFB
  (Float, Float, Float, Float, Float, Float) = {
    //changes on continue operator
    var currentOperationNumber = 1
    val endNumber = 7
    def assignOperationNumber(operationToMatch : () => Float, on_low : Int, on_eq : Int, on_gt: Int) : Unit = {
      operationToMatch() match {
        case a if a  < 0 =>
          currentOperationNumber = on_low
        case a if a == 0 =>
          currentOperationNumber = on_eq
        case a if a > 0 =>
          currentOperationNumber = on_gt
      }
    }
    def assignOperationNumberN(some : Int) : Unit = currentOperationNumber=some

    var KFALF = 1f
    var DELF : Float = 0f
    var WFV = 0f
    var BW = 0f
    var KFV = 0f
    var Z = 0
    var X = 0f
    var SCHET = 0
    var PSIDR = 0f
    var Z0_LOCAL = Z0
    var SXEM = 0
    var KFB_LOCAL = KFB
    var WFT = 0f
    var ZV = 0f
    var YF = 0f
    var YBET = 0f
    var SGF = 0f
    var SGFM = 0f
    var SGF1 = 0f; var SGF2 = 0f
    var SGFM1 = 0f;var SGFM2 =0f

    if (EPBET > 1) KFALF = (4f + (EPALF -1f)*(ST-5))/4f/EPALF
    DELF = 0.016f
    if (abs(HG) > 0)DELF = 0.011f
    if (BET - 0.1 > 0) DELF =0.006f
    WFV = DELF*G0*V*sqrt(AW/ U).toFloat
    if (WFV > WV) WFV =WV
    BW = BW1
    KFV = 1f + WFV*BW/FT
    Z=Z1
    X = X1
    SCHET = 0
    while(currentOperationNumber != endNumber){
      currentOperationNumber match {
        //НАЧАЛО ЦИКЛА
        case 1 =>
          breakable{
            if (KFB > 1) {
              currentOperationNumber = 4
              break;
            }
            PSIDR = PSIBD
            if (NWR > 1)PSIDR=PSIBD/NWR
            assignOperationNumber(() => HRC2-35f, 2,2,3)
          }
        case 2 =>
          if (HRC1 <= 35f) SXEM = 4
          if (HRC1 > 35f) SXEM = 3
          if (CONSOL == 1) SXEM =2
          KFB_LOCAL = 1f + 1.1f / SXEM * PSIDR
          assignOperationNumberN(4)
        case 3 =>
          if (HRC1 <= 45f) SXEM = 4
          if (HRC1 > 45f) SXEM = 3
          if (CONSOL == 1) SXEM =2
          KFB_LOCAL = 1f + 1.8f / SXEM * PSIDR
          assignOperationNumberN(4)
          // ШЕСТЕРНЯ
        case 4 =>
          WFT = FT * KFALF * KFB * KFV / BW
          assignOperationNumberN(5)
        case 5=>
          breakable{
            ZV = abs(Z / pow(cos(BET), 3).toFloat)
            YF = 3.6f*(1f+ (112f*pow(X, 2).toFloat- 154f*X + 71f)/pow(ZV, 2).toFloat-(2.8f*X+0.93f)/ZV)
            if (Z < abs(Z)) YF = 4.3f-8f/pow(Z0, 0.8f).toFloat*(1f+0.23f*X)-0.33f*Z0*1.0e1f-4f*(180f-ZV)*(1f+56f*X/pow(Z0, 0.8).toFloat)
            YBET = 1f - BET/140f*180/Pi.toFloat
            if (YBET < 0.7f) YBET=0.7f
            SGF=YEP * YBET * YF * WFT / M
            SGFM = SGF * TTED
            if (SCHET > 0.5) {
              assignOperationNumberN(6)
              break
            }
            SGF1 = SGF
            SGFM1 = SGFM
            //КОЛЕСО
            if (SIGN < 0f) Z = -Z2
            if (SIGN > 0) Z=Z2
            if (Z0_LOCAL <= 0) Z0_LOCAL = 40
            X=X2
            BW = BW2
            SCHET = SCHET + 1
            assignOperationNumberN(4)
          }
        case 6 =>
          SGF2 = SGF
          SGFM2 = SGFM
          assignOperationNumberN(7)
        case 7 =>
          throw new IllegalStateException("Infinity loop in DOPN")
      }
    }
    //SGF1, SGF2, SGFM1, SGFM2, KFV, KFB
    (SGF1, SGF2, SGFM1, SGFM2, KFV, KFB)
  }
}
