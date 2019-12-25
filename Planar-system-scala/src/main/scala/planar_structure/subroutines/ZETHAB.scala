package planar_structure.subroutines

import scala.collection.mutable.ArrayBuffer
import scala.math._

import util.control.Breaks._
/**
 *
 * @param ZA расчитанное число зубьев центральной шестерни
 * @param ZG расчитанное число зубьев сателлита
 * @param ZB расчитанное число зубьев эпицикла
 * @param XA смещение шестерни
 * @param XG смещение колеса сателлита
 * @param XB смещение колеса эпицикла
 * @param ALFWAG угол зацепления между центральной шестерней и сателлитом
 * @param ALFWGB угол зацепления между   сателлитом и центральным колесом-эпициклом
 * @param ALFT торцовый угол профиля зуба
 * @param DELYA коэффициент уравнительного смещения в зацеплении между колесами а и g
 * @param DELYB коэфф-т уравнительного смещения в зацеплении м/у колесами g и b
 */
case class Wheel3Numbers(ZA : Int, ZG : Int, ZB : Int, XA: Float, XG : Float, XB : Float,
                         ALFWAG : Float,
                        ALFWGB : Float, //
                         ALFT : Float, //
                         DELYA : Float, //
                         DELYB : Float //
                        )
/**
 *
 * @param ZA расчитанное число зубьев центральной шестерни
 * @param ZG расчитанное число зубьев сателлита
 * @param ZF рассчитанное число второго венца сателлита
 * @param ZB расчитанное число зубьев эпицикла
 * @param XA смещение шестерни
 * @param XG смещение колеса сателлита
 * @param XF смещение второго колеса сателита
 * @param XB смещение колеса эпицикла
 * @param ALFWAG угол зацепления между центральной шестерней и сателлитом
 * @param ALFWFB угол зацепления между   сателлитом и центральным колесом-эпициклом
 * @param ALFT торцовый угол профиля зуба
 * @param DELYA коэффициент уравнительного смещения в зацеплении между колесами а и g
 * @param DELYB коэфф-т уравнительного смещения в зацеплении м/у колесами g и b
 */
case class Wheel4Numbers(ZA : Int, ZG : Int, ZF :Int, ZB : Int, XA: Float, XG : Float, XF : Float,
                         XB : Float,
                         ALFWAG : Float,
                         ALFWFB : Float, //
                         ALFT : Float, //
                         DELYA : Float, //
                         DELYB : Float //
                        )
/**
 * при расчете W3 считаются числа зубьев для схемы 2k-h с входной центральной шестерней и выходным водилом
 */
object ZETHAB {
  val U_ACCURACY = 0.04
  /**
   * @note минимальное число зубьев на центральной шестерне
   */
  val ZA_MIN : Short = 12
  /**
   * @note функция используется для определения рекомендуемого максимального значения числа зубьев
   *       центральной входной шестерни в зависимости от твердости её материала
   *       при этом условием отсутствия подрезания считают число зубьев больше 12
   * @param HRC твердость материала зуба колеса.
   * @return
   */
  def getZAMAX_by_HRC(HRC : Float) : Short = {
    HRC match {
      case a if a <= 35f => 24
      case a if (a > 35f) && a <=52f => 21
      case a if (a > 52f) => 18
    }
  }

  def SIMPLE_NUMBER(z : Int) : Boolean = {
    if (
      (z / 4f - (z / 4f).toInt == 0) ||
        ( z/3f - (z/3f).toInt == 0) ||
        (z/5f - (z/5f).toInt == 0) ||
        (z / 7f - (z / 7f).toInt ==0)
    ) true else false
  }
  class B_Iterator(initial_ZB : Int) extends Iterator[Int]{
    var counter : Int = 0
    override def hasNext: Boolean = if (counter > 4) false else true
    //Варьируем число
    override def next(): Int = counter match {
      case 0 =>
        counter+=1
        initial_ZB
      case 1 =>
        counter+=1
        initial_ZB+1
      case 2 =>
        counter+=1
        initial_ZB-1
      case 3 =>
        counter+=1
        initial_ZB+2
      case 4 =>
        counter+=1
        initial_ZB-2
    }
  }
  class C_ITERATOR()
  /**
   * @note если xa и xg == 0, то при возникновении подрезания они высчитываются
   *       в противном случае (>0) остаются фиксированными при выполнении программы
   * @param Uab передаточное отношение при остановленном водиле
   * @param Za_max максимальное число зубьев центральной шестерни
   * @param nw число потоков (сателлитов)
   * @param xa заданное смещение колеса а
   * @param xb заданное смещение колеса b
   * @param xg заданное смещение колеса g
   * @param beta угол наклона зубьев
   * @param alpha
   * @param ha
   * @param hl
   */
  def W3(Uab : Float, //
         Za_max : Int,
         nw : Int, //
         xa : Float, //
        xb : Float,
         xg : Float, //заданное смещение колеса g
         beta : Float,
         alpha : Float = ChangeableParameters.ALF,
         ha : Float = ChangeableParameters.HA,
         hl : Float = ChangeableParameters.HL) : Wheel3Numbers = {
    val POSITIVE_U = abs(Uab)
    val CALCULATED  = ArrayBuffer.empty[Wheel3Numbers]
    //скорректируем число максимальное число зубьев по условию кратности числа зубьев центр. шестерни количеству
    //сателлитов
    var za_corr : Int = Za_max
    while (za_corr % nw != 0){
      za_corr = za_corr - 1
    }
    //сначала организуем итератор по числам центральной шестерни
    val ZA = new Iterator[Int]{
      var number: Int = za_corr + nw
      override def hasNext: Boolean = if (number - nw > ZA_MIN) true else false
      override def next(): Int = {number -= nw; number}
    }
    ZA.foreach{za =>
      println("In za")
      //создаем zb в соответствии с передаточным числом и округляем его
      val zb = new B_Iterator(za*(POSITIVE_U).toInt)
      //ПРОВЕРКА zb НА ПРОСТОЕ ЧИСЛО
      zb.foreach(zb =>
        if (SIMPLE_NUMBER(zb)){
          //находим итоговое передаточное отношение
          val UahR = zb/za.toFloat
          //если заданная точность выполняется
          if ((1-U_ACCURACY)*POSITIVE_U <= UahR && UahR <= POSITIVE_U*(1+U_ACCURACY)){
              //ПРОВЕРКА УСЛОВИЙ СБОРКИ
            val CEL1 = (zb + za) / nw.toFloat
                //если разница кратна числу сателлитов
            if (CEL1 - CEL1.toInt == 0){
                //УСЛОВИЕ СОСЕДСТВА
              //println("assembly")
              var zg = ((zb-za)/2).toInt //число зубьев на венце сателлита
              val ZAG = (za + zg)*sin(Pi / nw) //зубное расстояние между соседними сателлитами
              val ALFT = atan(tan(alpha) / cos(beta))
              val INVT = tan(ALFT) - ALFT
              //если диаметр колеса сателлита меньше расстояния между соседними сателлитами
              if (zg + 2 - ZAG < 0){
                  //СМЕЩЕНИЕ ИСХОДНОГО КОНТУРА
               // println("neighborhood")

                var xa_ = xa; var xb_ = xb; var xg_ = xg
                var XSAG = xa + xg //суммарное смещение шестерни и колеса
                if (xa == 0 && xg == 0 && xb == 0 ){
                  val Z1R = 2 * (hl - ha - 0.3) * cos(beta) / pow(sin(ALFT),2) + 2
                  if (za <= 17 && za >= Z1R) xa_ = 0.3f
                  if (za < Z1R) xa_ = 0.5f
                  if (zg <= 17 && zg >= Z1R) xg_ = 0.3f
                  if (zg < Z1R) xg_ = 0.5f
                }
                  //ОПРЕДЕЛЕНИЕ УГЛОВ ЗАЦЕПЛЕНИЙ
                val INVAG = XSAG*2 * tan(ALFT)/ (za + zg) + INVT
                var ALFR = 1.31611f*pow(INVAG,0.290865f).toFloat-0.03806f
                if (XSAG != 0){
                  var INVR = tan(ALFR) - ALFR
                  while (INVAG - INVR < 0){
                    ALFR = ALFR - 2e-6f
                    INVR = tan(ALFR) - ALFR
                  }
                }
                val ALFWAG = ALFR
                val Y = (zb - zg) / (za + zg) * cos(ALFWAG)
                val ALFWGB = atan(sqrt(1 - pow(Y,2))/ Y).toFloat
                val INVGB = tan(ALFWGB) - ALFWGB
                xb_ = (INVGB - INVT).toFloat*(zb-zg)/2/tan(alpha).toFloat + xg
                breakable {
                  //ПРОВЕРКА УСЛОВИЯ xb >= 0 для смещения колеса эпицикла
                  if (xb < 0) {
                    if (xa - 0.3 <= 0) {
                      xa_ = xa_ - xb_
                      break()
                      //TODO здесь в учебнике идет перерасчет с момента определения суммы смещений, у меня
                      //же просто все сбросится - не очень хорошо
                    } else {
                      zg = zg + 1
                    }
                  }
                  val DELYA = XSAG - (cos(ALFT) / cos(ALFWAG) - 1).toFloat * (za + zg) / 2
                  val DELYB = xb_ - xg_ - (cos(ALFT) / cos(ALFWGB) - 1).toFloat * (zb - zg) / 2
                  CALCULATED.addOne(
                    Wheel3Numbers(za, zg, zb, xa_, xg_, xb_, ALFWAG, ALFWGB, ALFT.toFloat, DELYA, DELYB))
                }
              }
            }
          }
        }
      )
    }
    CALCULATED(0)
  }
/*
  /**
   * @note если xa и xg == 0, то при возникновении подрезания они высчитываются
   *       в противном случае (>0) остаются фиксированными при выполнении программы
   *       функция подходит для схема TIPRE 12 (рис 4.1 в)
   *       или схема с внешним-внутренним зацеплением
   * @param Uab передаточное отношение при остановленном водиле
   * @param Za_max максимальное число зубьев центральной шестерни
   * @param nw число потоков (сателлитов)
   * @param xa заданное смещение колеса а
   * @param xb заданное смещение колеса b
   * @param xg заданное смещение колеса g
   * @param xf заданное смещение колеса f
   * @param beta угол наклона зубьев
   * @param alpha
   * @param ha
   * @param hl
   */
  def W4_ext_int(Uab : Float, //
         Za_max : Int,
         nw : Int, //
         xa : Float, //
         xb : Float,
         xg : Float, //заданное смещение колеса g
        xf: Float,
         beta : Float,
         alpha : Float = ChangeableParameters.ALF,
         ha : Float = ChangeableParameters.HA,
         hl : Float = ChangeableParameters.HL) : Wheel4Numbers = {

    val POSITIVE_U = abs(Uab)
    val CALCULATED  = ArrayBuffer.empty[Wheel3Numbers]
    //скорректируем число максимальное число зубьев по условию кратности числа зубьев центр. шестерни количеству
    //сателлитов
    var za_corr : Int = Za_max
    while (za_corr % nw != 0){
      za_corr = za_corr - 1
    }
    //сначала организуем итератор по числам центральной шестерни
    val ZA = new Iterator[Int]{
      var number: Int = za_corr + nw
      override def hasNext: Boolean = if (number - nw > ZA_MIN) true else false
      override def next(): Int = {number -= nw; number}
    }

    /*val zb = new B_Iterator(za*(POSITIVE_U).toInt)*/

  }*/


}
