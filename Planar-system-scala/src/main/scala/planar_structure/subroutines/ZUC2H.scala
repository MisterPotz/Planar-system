package planar_structure.subroutines
import util.control.Breaks._

import math._
/**
 * расчет контактных напряжений зубчатых передач
 */
object ZUC2H {


  def ZUC2H(Z1 : Int, Z2 : Int, X1 : Float, X2 : Float, HRC1 : Float, HRC2 : Float, AW : Float,
            M : Float, BW:Float, U : Float, N2 : Float, SIGN : Int, T2 : Float, TTED : Float, NWR : Int,
           //-----------------
            HG : Float, BET: Float, ALF : Float, ALFT : Float, ALFTW: Float, EPALF : Float,
            E1 : Float, E2 : Float, PUAS : Float, ST: Int=0, CONSOL : Float=1, KHB : Float =1 ) :
  (Float, Float, Float, Float, Float, Float, Float,Float, Float, Float, Float, Float, Float, Float,Float, Float) =
  //V, PSIBD, SGH, SGHM, DW1, DW2, FT, FA, FR, KHV
  //------
  //EPBET, G0. WV, AKA, ST_LOCAL, KHB_LOCAL
   {
     //changes on continue operator
     var currentOperationNumber = 0
     val endNumber = 12
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

     var DW1 : Float = 2f * AW / (U + SIGN)
     var DW2 : Float = DW1*U
     var V : Float = (Pi * DW2 *N2).toFloat / 6.0e4f
     var ST_LOCAL : Int = ST
     var PSIDR : Float = 0f
     var PSIBD : Float = 0f
     var SXEM : Float = 0
     var KHALF : Float = 0f
     var KHB_LOCAL : Float = KHB
     var DELH : Float = 0f
     var G0  : Float = 0f
     var WHV  : Float = 0f
     var WV  : Float = 0f
     var FT  : Float = 0f
     var FA  : Float = 0f
     var FR  : Float = 0f
     var KHV : Float = 0f
     var EPBET  : Float = 0f
     var ZEP  : Float = 0f
     var ZM : Float = 0f
     var SINBTB  : Float = 0f
     var AKA : Float = 0f
     var BETB  : Float = 0f
     var Y1  : Float = 0f
     var ZH  : Float = 0f
     var WHT  : Float = 0f
     var SGH : Float = 0f
     var SGHM  : Float = 0f

     while(currentOperationNumber != endNumber){
       currentOperationNumber match {
         //НАЧАЛО ЦИКЛА
         case 0 => breakable{
           if (ST_LOCAL > 0 && BET > 0.01f) {
             currentOperationNumber = 4
             break
           }
           assignOperationNumber(() => (BET - 0.01f), 1,1,3)
         }
         case 1 => breakable {
           if (ST_LOCAL > 0) {
             currentOperationNumber = 2
             break
           }
           ST_LOCAL = (10.1f - 0.2f*V).toInt
           assignOperationNumberN(2)
         }
         case 2=>
           KHALF = 1f
           assignOperationNumberN(5)
         case 3=>
           ST_LOCAL = (10.1f - 0.12f*V).toInt
           assignOperationNumberN(4)
         case 4 =>
           KHALF = ((0.0026f*ST_LOCAL - 0.013)*V + 0.027f*ST_LOCAL+0.84).toFloat
           assignOperationNumberN(5)
         case 5=>
           PSIBD = BW / DW1
           PSIDR = PSIBD / NWR
           breakable{
             if (KHB > 1) {
               assignOperationNumberN(8)
               break;
             }
             assignOperationNumber(() => (HRC2-35f), 6,6,7)
           }
         case 6=>
           if (HRC1 <= 35f) SXEM = 4
           if (HRC1 > 35) SXEM = 3
           if (CONSOL == 1) SXEM =2
           KHB_LOCAL=1f + (0.51f*PSIDR / SXEM).toFloat
           assignOperationNumberN(8)
         case 7 =>
           if (HRC1 <= 45f) SXEM = 4
           if (HRC1 > 45) SXEM = 3
           if (CONSOL == 1) SXEM = 2
           KHB_LOCAL = 1f + 1.1f*PSIDR / SXEM
           assignOperationNumberN(8)
         case 8 =>
           assignOperationNumber(() => HRC2 - 35f,9,9,10)
         case 9 =>
           if (BET < 0.01f) DELH = 0.006f
           if (abs(HG) > 0) DELH = 0.004f
           if (BET > 0.01f) DELH = 0.002f
           assignOperationNumberN(11)
         case 10 =>
           if (BET < 0.01f) DELH = 0.014f
           if (abs(HG) > 0) DELH = 0.01f
           if (BET > 0.01f) DELH = 0.004f
           assignOperationNumberN(11)
         case 11 =>
           G0 = 10f * ST_LOCAL + M * ((0.16f * ST_LOCAL - 0.78f)*pow(ST_LOCAL, 0.4f)+0.58).toFloat -22f
           WHV = DELH * G0 * V * sqrt(AW / U).toFloat
           WV = M * (4f*ST_LOCAL - 15f) + 0.1f*pow(ST_LOCAL, 4).toFloat
           if (WHV > WV) WHV =WV
           FT=2000f * T2/ DW2
           FR = (FT * sin(ALFTW) / cos(ALFTW)).toFloat
           FA = (FT * sin(BET) / cos(BET)).toFloat
           KHV = 1f + WHV*BW / FT
           EPBET = (BW * sin(BET)/ Pi / M).toFloat
           ZEP = sqrt((4f- EPALF) / 3f).toFloat
           if (EPBET >= 0.9f) ZEP =sqrt(1f/ EPALF).toFloat
           ZM=sqrt(2f*E1 / (E1+E2)/Pi*E2 /(1f - pow(PUAS,2))).toFloat
           SINBTB = sin(BET).toFloat* cos(ALF).toFloat
           Y1 = SINBTB
           BETB = atan(Y1 / sqrt(1f- pow(Y1, 2))).toFloat
           ZH = sqrt(2 *cos(BETB) / sin(2 * ALFTW)).toFloat
           WHT = FT * KHB * KHV * KHALF / BW
           SGH = ZH * ZM * ZEP * sqrt(WHT/DW1*(U + SIGN) / U).toFloat
           SGHM = SGH *sqrt(TTED).toFloat
           AKA = pow((2000f / 4f *pow(ZH*ZM*ZEP,2)*KHALF), 0.33).toFloat
           assignOperationNumberN(12)
         case 12 =>
           throw new IllegalStateException("Infinity loop in DOPN")
       }
       }
     //V, PSIBD, SGH, SGHM, DW1, DW2, FT, FA, FR, KHV

     (V, PSIBD, SGH, SGHM, DW1, DW2, FT, FA, FR, KHV,
       //------
       //EPBET, G0. WV, AKA
       EPBET, G0, WV, AKA, ST_LOCAL, KHB_LOCAL)
   }

}
