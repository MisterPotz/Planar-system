package planar_structure.subroutines
package planar_structure.subroutines
import scala.math.{Pi, abs, atan, cos, pow, sin, sqrt,tan}
import scala.util.control.Breaks.{break, breakable}

/**
 * in FOrtran doesn't use common block - so here nothing except the KPD, PSIPD, PSISUM is returned
 */
object KPDPL {
  def KPDPL(A1R : Float, A2R: Float, B1R : Float, B2R : Float, D1R : Float, D2R: Float, D3R : Float,
            D4R : Float, D5R : Float, D6R : Float, TVL1 : Float, TVL2: Float, TVL3 : Float, NV1 : Float,
            NV2 : Float, NV3 : Float, URE : Float, UB : Float, UT:  Float, OMEG : Float, NW : Float,
            PSIZ : Float, PSIZB : Float, PSIZT : Float, TIPRE : Float, GAM: Float, FTRPD : Float) :
  //KPD, PSIPD, PSISUM
  (Float, Float, Float) = {
    //changes on continue operator
    var currentOperationNumber = 1
    val endNumber = 14
    def assignOperationNumber(operationToMatch : () => Float, on_low : Int, on_eq : Int, on_gt: Int) : Unit = {
      operationToMatch() match {
        case a if a < 0 =>
          currentOperationNumber = on_low
        case a if a == 0 =>
          currentOperationNumber = on_eq
        case a if a > 0 =>
          currentOperationNumber = on_gt
      }
    }
    def assignOperationNumberN(some : Int) : Unit = currentOperationNumber=some

    var PSIZ_L = PSIZ
    var PSISUM = 0f
    var D5RR = 0f
    var UAH = UB
    var RSIZ = PSIZB
    var DP= 0.28f * D2R
    var D2RR = D2R
    var TH = TVL2
    var NH = NV2
    var NAH = NV1 - NV2
    var AR = A1R
    var UAG = D1R / D2R
    var KPDB = 0f
    var KPD = 0f
    var FT = 0f
    var NAM = 0f
    var KPDT = 0f
    var NGH = 0f
    var MAS = 0f
    var FCEN = 0f
    var FSUM = 0f
    var TTREN = 0f
    var PSIPD = 0f

    var SCHET = 1
    //РЕДУКТОРЫ "A-HA", "A2-HA"
    assignOperationNumber(() => TIPRE -11, 1,1,5)
    while(currentOperationNumber != endNumber){
      currentOperationNumber match {
        //НАЧАЛО ЦИКЛА
        case 1 =>
          D5RR = 0f
          UAH = UB
          RSIZ = PSIZB
          DP= 0.28f * D2R
          D2RR = D2R
          assignOperationNumberN(2)
        case 2 =>
          TH = TVL2
          NH = NV2
          NAH = NV1 - NV2
          AR = A1R
          UAG = D1R / D2R
          assignOperationNumberN(11)
        case 3 =>
          KPDB = 1f - (UAH - 1f) / UAH * PSISUM
          KPD = KPDB
          breakable{
            if (TIPRE != 11) {
              assignOperationNumberN(14)
              break
            }
            //ТИХОХОДНАЯ СТУПЕНЬ
            SCHET = SCHET + 1
            D2RR = 0
            UAH = UT
            PSIZ_L = PSIZT
            DP= 0.28f * D5R
            D5RR = D5R
            TH = TVL3
            NH = NV3
            NAH = NV2 - NV3
            AR = A2R
            UAG= D4R /D5R
            assignOperationNumberN(11)
          }
        case 4 =>
          KPDT = 1f - (UAH - 1f) / UAH * PSISUM
          KPD = KPDB * KPDT
          assignOperationNumberN(14)
          //РЕДУКТОР ОДНОСТУПЕНЧАТЫЙ "B-HA"
        case 5=>
          assignOperationNumber(() => TIPRE - 13, 6,7,9)
        case 6 =>
          DP= 0.5f*D5R
          UAH = URE
          assignOperationNumberN(12)
          //РЕДУКТОР ОДНОСТУПЕНЧАТЫЙ "3K-EA <D3R > D6R>
        case 7 =>
          FT = 2f * TVL2 /D6R/NW * OMEG*2f
          DP = 0.3f * D5R
          TH = TVL2
          NH = NV1 / (1f + D3R / D1R)
          NAM = NV1 / (1f + D1R / D3R)
          AR = A1R
          UAG = D1R / D2R
          assignOperationNumberN(12)
        case 8 =>
          KPD = 0.98f  / (1f + (URE / (1f+D3R/D1R) -1f) * PSISUM)
          assignOperationNumberN(14)
          //РЕДУКТОР ОДНОСТУПЕНЧАТЫЙ "C-BH"
        case 9 =>
          FT = 2f*TVL2 / D6R/NW*OMEG*2
          DP = 0.28f * D5R
          TH = TVL2
          NH = NV2
          NAH = NV1 - NV2
          UAG = D6R / D5R
          assignOperationNumberN(12)
        case 10 =>
          KPD = 1f / (1f + abs(1f - URE) * PSISUM)
          assignOperationNumberN(11)
          //ПОТЕРИ В ПОДШИПНИКАХ САТЕЛЛИТОВ С
          //УЧЕТОМ ЦЕНТРОБЕЖНЫХ СИЛ
        case 11 =>
          FT = TH / AR / NW * OMEG *1000f
          assignOperationNumberN(12)
        case 12 =>
          NGH = NAH * UAG
          MAS = (Pi /4f*(pow(D2RR,2)*B1R + pow(D5RR,2)*B2R) * GAM * 1.0e-6f).toFloat
          FCEN = MAS * AR * pow((Pi *NH /30),2).toFloat / 981f
          FSUM = sqrt(pow(2 * FT,2)+pow(FCEN,2)).toFloat
          TTREN = NW * FTRPD * FSUM * DP /2/1000
          PSIPD = TTREN * NGH / (TH*NH)
          PSISUM = PSIZ + PSIPD
          breakable{
            if (SCHET > 1){
              assignOperationNumberN(4)
              break()
            }
            assignOperationNumber(() => TIPRE - 11,3,3,13)
          }
        case 13 =>
          assignOperationNumber(() => TIPRE - 13, 3, 8, 10)
        case 14=>
          throw new IllegalStateException("Infinity loop in DOPN")
      }
    }
    //KPD, PSIPD, PSISUM
    (KPD, PSIPD, PSISUM)
  }
}
