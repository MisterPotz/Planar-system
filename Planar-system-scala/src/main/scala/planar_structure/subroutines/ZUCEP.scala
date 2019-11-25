package planar_structure.subroutines
import scala.math.{Pi, abs, atan, cos, pow, sin, sqrt,tan}
import scala.util.control.Breaks.{break, breakable}

/**
 * DELY вводится после перебора зубьев
 */
object ZUCEP {
  def ZUCEP(Z1 : Int, Z2 : Int, X1 : Float, X2 : Float, M : Float, SIGN : Int, N2 : Float, PAR : Int,
           //------------------------
           HA : Float, HL :Float, C:Float, ROF : Float, HG : Float, BET :Float, ALF : Float, ALFT : Float,
            ALFTW : Float, EPMI : Float, D0 : Float, DELY : Float, Z0: Int = 0,X0: Float =0 , DA0 : Float=0) :
  //AW, DB1, DA1, DF1, DB2, DA2, DF2, CF1, CF2, PSIZC
    //------
    //HK1, HK2, DELY, EA1, EA2, EAM, EPALF
  (Float, Float, Float, Float, Float, Float,Float,Float,Float,Float,
    Float, Float, Float, Float, Float, Float, Float) = {
    //changes on continue operator
    var currentOperationNumber = 1
    val endNumber = 17
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

    var K2 : Float = 0f
    var Z0_LOCAL : Int = Z0
    var DA0_LOCAL : Float = DA0
    var X0_LOCAL : Float = X0
    var INVAT : Float = 0f
    var INVW = 0f
    var ALFR = 0f
    var INVAR = 0f
    var ALFW02= 0f
    var AW02 = 0f
    var Y = 0f
    var ALFA0 =0f
    var DK1 = 0f
    var DK2 = 0f
    var ALFK1 = 0f
    var ROP1 = 0f
    var ROP2 = 0f
    var ALFK2 =0f
    var ROL1 = 0f
    var ROL2 = 0f
    var CF2 = 0f
    var CF1 = 0f
    var EA1 =0f; var EA2 = 0f
    var EPALF = 0f
    var EAM = 0f
    var V2 = 0f
    var VS = 0f
    var FTRZ = 0f
    var PSIZR = 0f
    var PSIZC = 0f

    var HK1 = HG
    var HK2 = HG
    var A = (Z2 + SIGN * Z1) * M /2/cos(BET).toFloat
    var AW = A* cos(ALFT).toFloat / cos(ALFTW).toFloat
    var D1 = Z1 * M /cos(BET).toFloat
    var DB1 : Float = D1*cos(ALFT).toFloat
    var D2 = Z2 * M / cos(BET).toFloat
    var DB2 = D2 * cos(ALFT).toFloat
    var XSUM = X2 + SIGN*X1
    var DELY_LOCAL = DELY
    if (PAR <= 0) DELY_LOCAL = XSUM - (AW - A) / M
    var DA1 = D1 + 2f * (HA + X1 - DELY) * M
    var DF1 = D1 - 2f * (HA + C -X1)* M
    var DA2 = D2 + 2f * (HA + X2 - DELY) * M
    var DF2 = D2 - 2f * (HA + C  - X2) * M

    assignOperationNumber(() => SIGN, 1, 7,7)
    while(currentOperationNumber != endNumber){
      currentOperationNumber match {
        //НАЧАЛО ЦИКЛА
        case 1 =>
          K2 = 0.25f - 0.125f*X2
          if (X2  >= 2) K2 = 0
          DA2 = D2 - 2f * (HA -X2 +DELY_LOCAL - K2)* M
          DF2 = D2 + 2f * (HA +C + X2) * M
          assignOperationNumber(() => Z0_LOCAL -1, 2,3,3)
        case 2 =>
          Z0_LOCAL = (0.3f * Z2).toInt
          DA0_LOCAL = M * (Z0_LOCAL + 2f * (HA + C + X0_LOCAL))
          assignOperationNumberN(3)
        case 3 =>
          INVAT = tan(ALFT).toFloat - ALFT
          //УГОЛ СТАНОЧНОГО ЗАЦЕПЛЕНИЯ
          INVW = 2 * (X2- X0_LOCAL)* tan(ALF).toFloat / (Z2 - Z0_LOCAL) + INVAT
          ALFR = 1.316111f * pow(INVW, 0.29085).toFloat - 0.03806f
          assignOperationNumberN(4)
        case 4=>
          INVAR = tan(ALFR).toFloat - ALFR
          assignOperationNumber(() => INVW - INVAR, 5,6,6)
        case 5=>
          ALFR = ALFR - 2e-6f
          assignOperationNumberN(4)
        case 6=>
          ALFW02 = ALFR
          AW02 = (M * (Z2-Z0_LOCAL) /2 / cos(BET) * cos(ALFT) / cos(ALFW02)).toFloat
          DF2 = 2f * AW02 + DA0_LOCAL
          Y = M *Z0 * cos(ALFT).toFloat /DA0_LOCAL
          ALFA0=atan(sqrt(1 - pow(Y, 2)) / Y).toFloat
          assignOperationNumberN(7)
        case 7 =>
        //ВЕЛИЧИНА УМЕНЬШЕНИЯ ВЫСОТЫ ЗУБА HK1, HK2 при
        //интерференции во впадине
          breakable {
            DK1 = DA1 - 2f * HK1 * M
            DK2 = DA2 - SIGN * 2f * HK2 * M
            ALFK1 = atan(sqrt(1f - pow((DB1 /DK1),2))/(DB1 / DK1)).toFloat
            ROP1 = (-SIGN * DB2 / 2f*sin(ALFK2) / cos(ALFK2) + SIGN * AW *sin(ALFTW)).toFloat
            ROP2 = (-SIGN * DB1 / 2f*sin(ALFK1) / cos(ALFK1) + SIGN * AW *sin(ALFTW)).toFloat
            ROL1 = (D1 * sin(ALFT) /2f - (HL - HA - X1)* M / sin(ALFT)).toFloat
            ROL2 = (D2 * sin(ALFT)/2f - SIGN * (HL-HA-SIGN*X2)*M/sin(ALFT)).toFloat
            if (SIGN < 0)ROL2 = (AW02*sin(ALFW02)+DA0/2f*sin(ALFA0)).toFloat
            CF2 = SIGN * (AW-DF2/2f) - DK1 / 2
            CF1 = SIGN * (AW-DK2/2f)-DF1/2
            if (ROL1 < ROP1 && CF1 > 0.15f*M){
              assignOperationNumberN(8)
              break
            }
            HK2=HK2 + 0.05f*M
            if (HK2 > 0.2f * M){
              assignOperationNumberN(9)
              break()
            }
            assignOperationNumberN(7)
          }
        case 8 =>
          breakable{
            if (SIGN * (ROL2 - ROP2) <= 0 && CF2 > 0.15*M) {
            assignOperationNumberN(11)
            break
            }
            if (PAR == 1 && SIGN < 0){
              assignOperationNumberN(9)
              break
            }
            HK1 = HK1 + 0.05f*M
            if (HK1 > 0.2 * M) {
              assignOperationNumberN(9)
              break()
            }
            assignOperationNumberN(7)
          }
        case 9 =>
          println("9 : ZUCEP")
          assignOperationNumberN(10)
        case 10 =>
          breakable{
            if (HK1 > 0 || PAR == 1) {
            assignOperationNumberN(11)
            break
          }
          assignOperationNumberN(8)
          }
        case 11 =>
          EA1 = (Z1 * (tan(ALFK1) - tan(ALFTW))/2/Pi).toFloat
          EA2 = (Z2 * (SIGN*tan(ALFK2) - SIGN* tan(ALFTW))/2/Pi).toFloat
          EPALF = EA1 + EA2
          EAM = EA1
          if (EA2 > EA1) EAM =EA2
          assignOperationNumber(() => EPALF - EPMI, 12,14,14)
        case 12 =>
          println("12 : ZUCEP")
          assignOperationNumberN(13)
        case 13 =>
          ///DO NOTHING
          assignOperationNumberN(14)
        case 14 =>
          DA1 = DK1
          DA2 = DK2
          assignOperationNumberN(16)
        //КОЭФФИЦИЕНТ ПОТЕРЬ В ОДНОМ ЗАЦЕПЛЕНИИ
        case 16 =>
          V2 = (Pi * D2 * N2 / 6000*cos(ALFT) / cos(ALFTW)).toFloat
          VS = (2 * V2 * sin(ALFTW)).toFloat
          FTRZ  = (1.25f*(0.102f-0.02f*pow(VS, 0.33333))).toFloat
          PSIZR = (2 * Pi * FTRZ * (1f/Z1.toFloat + SIGN * 1f/ Z2.toFloat)).toFloat
          PSIZC=(PSIZR*(1f-EPALF +pow(EPALF,2)/2f)*EAM / EPALF).toFloat
          if (BET > 0.1f)PSIZC=(PSIZR/2f/cos(BET)*(pow(EA1,2)+pow(EA2,2))/EPALF).toFloat
          assignOperationNumberN(17)
        case 17=>
          throw new IllegalStateException("Infinity loop in DOPN")
      }
    }
    //AW, DB1, DA1, DF1, DB2, DA2, DF2, CF1, CF2, PSIZC

    (AW, DB1, DA1, DF1, DB2, DA2, DF2, CF1, CF2, PSIZC,
      //HK1, HK2, DELY, EA1, EA2, EAM, EPALF
      HK1, HK2, DELY, EA1, EA2, EAM, EPALF
    )
  }
}
