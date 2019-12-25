package planar_structure.subroutines

import com.sun.jdi.connect.Connector.BooleanArgument

import scala.math.Numeric.DoubleIsFractional

object CylindricGearTransmissionsCalculation {
  //1 - шестерня, 2 - колесо
  def getK(matrow1: StandardParameters.MaterialTableRow, matrow2: StandardParameters.MaterialTableRow): Double = {
    (matrow1, matrow2) match {
      case (a, b) if (a.outsideHB.mid <= 350 && b.outsideHB.mid <= 350) => 10
      case (a, b) if (a.outsideHB.mid >= 350 && b.outsideHB.mid <= 350) => 9
      case (a, b) if (a.outsideHB.mid >= 450 && b.outsideHB.mid <= 350) => 8
      case (a, b) if (a.outsideHB.mid >= 450 && b.outsideHB.mid >= 450) => 6
      case _ => 10
    }
  }

  def getSign(inner: Boolean = false) = if (inner) -1 else 1

  /**
   *
   * @param matrow1
   * @param matrow2
   * @param u     передаточное число от шестерни к колесу
   * @param T1    момент на шестерне
   * @param inner если внутреннее зацепление - то true
   */
  def getPreliminaryAW(matrow1: StandardParameters.MaterialTableRow, matrow2: StandardParameters.MaterialTableRow, u: Double,
                       T1: Double, inner: Boolean = false): Double = {
    val K = getK(matrow1, matrow2)
    val sign = getSign(inner)
    K * math.abs(u + sign * 1) * math.pow(T1 / u, 1 / 3.toFloat)
  }

  def getVelocity(matrow1: StandardParameters.MaterialTableRow, matrow2: StandardParameters.MaterialTableRow, u: Double,
                  T1: Double, n1: Double, inner: Boolean = false): Double = {
    val pre_aw = getPreliminaryAW(matrow1, matrow2, u, T1, inner)
    (2 * math.Pi * pre_aw * n1) / (6e4 * math.abs(u + getSign(inner) * 1))
  }

  /**
   * Леликов стр 17
   *
   * @param velocity скорость в зацеплении
   * @param bevel    косузбая ли передача
   * @return
   */
  def getAccuracyGrade(velocity: Double, bevel: Boolean = false): Double = {
    if (!bevel) {
      velocity match {
        case a if a <= 2 => 9
        case a if a <= 6 => 8
        case a if a <= 12 => 7
        case a if a <= 20 => 6
      }
    } else {
      velocity match {
        case a if a <= 4 => 9
        case a if a <= 10 => 8
        case a if a <= 20 => 7
        case a if a <= 30 => 6
      }
    }
  }

  /**
   * коэффициент неравномерности распределения нагрузки между сателитами
   *
   * @return
   */
  def getKW(): Double = {
    1.2
  }

  def getPsiBA(matrow1: StandardParameters.MaterialTableRow, matrow2: StandardParameters.MaterialTableRow): Double = {
    (matrow1, matrow2) match {
      case (a, b) if (a.outsideHB.mid <= 350 || b.outsideHB.mid <= 350) => 0.4
      case (a, b) if (a.outsideHB.mid <= 500 || b.outsideHB.mid <= 500) => 0.315
      case (a, b) if (a.outsideHB.mid >= 500 || b.outsideHB.mid >= 500) => 0.25
    }
  }





  def getWidth(psiBA: Double, aw: Double, wheelType: Int): Double = {
    wheelType match {
      case 1 => psiBA * aw
      case 0 => psiBA * aw + 3
      case -1 => 1.1 * (psiBA * aw + 3)
    }
  }

  //колесо
  def getKHV(materialTableRow: StandardParameters.MaterialTableRow,
             accuracyGrade: Int, velocity: Double, bevel: Boolean = false): Double = {
    if (materialTableRow.outsideHB.mid > 350) {
      accuracyGrade match {
        case 6 => if (!bevel) PolyTools.calculate(List(-4.9157593e-18, 1.0266097e-16, 2.0000000e-02, 1.0000000e+00), velocity)
        else PolyTools.calculate(List(1.22055138e-04, -1.94060150e-03, 1.55704261e-02, 9.96511278e-01), velocity)
        case 7 => if (!bevel) PolyTools.calculate(List(-6.11528822e-06, 4.33984962e-04, 2.14909273e-02, 9.96712782e-01), velocity)
        else PolyTools.calculate(List(-1.88817245e-18, 3.81021846e-17, 1.00000000e-02, 1.00000000e+00), velocity)
        case 8 => if (!bevel) PolyTools.calculate(List(-2.60437579e-18, 5.56136335e-17, 3.00000000e-02, 1.00000000e+00), velocity)
        else PolyTools.calculate(List(3.26315789e-05, -4.30526316e-04, 1.33557895e-02, 9.96147368e-01), velocity)
        case 9 => if (!bevel) PolyTools.calculate(List(-1.60802005e-04, 2.80511278e-03, 2.25647118e-02, 1.00405414e+00), velocity)
        else PolyTools.calculate(List(-8.94235589e-05, 1.51007519e-03, 7.78536341e-03, 9.99636090e-01), velocity)
      }
    } else {
      accuracyGrade match {
        case 6 => if (!bevel) PolyTools.calculate(List(3.26315789e-05, -4.30526316e-04, 3.33557895e-02, 9.96147368e-01), velocity)
        else PolyTools.calculate(List(1.15939850e-04, -1.50661654e-03, 1.70613534e-02, 9.93224060e-01), velocity)
        case 7 => if (!bevel) PolyTools.calculate(List(-3.61357141e-18, 5.07677560e-17, 4.00000000e-02, 1.00000000e+00), velocity)
        else PolyTools.calculate(List(8.94235589e-05, -1.51007519e-03, 2.22146366e-02, 1.00036391e+00), velocity)
        case 8 => if (!bevel) PolyTools.calculate(List(1.22055138e-04, -1.94060150e-03, 5.55704261e-02, 9.96511278e-01), velocity)
        else PolyTools.calculate(List(7.13784461e-05, -1.29503759e-03, 2.52206516e-02, 9.95581955e-01), velocity)
        case 9 => if (!bevel) PolyTools.calculate(List(-6.61253133e-04, 1.14354887e-02, 3.26486216e-03, 1.04143459e+00), velocity)
        else PolyTools.calculate(List(-1.22055138e-04, 1.94060150e-03, 1.44295739e-02, 1.00348872e+00), velocity)
      }
    }
  }

  //TODO KHbet0 забить как надо, уже нет времени с каждым коэффом маяться, поставлю заглушку
  def getKHBet0(psiBA: Double, schemeType: Int, matrow: StandardParameters.MaterialTableRow): Double = {
    1.1
  }

  def getKHW(v: Double, matrow: StandardParameters.MaterialTableRow): Double = {
    math.min(matrow.outsideHB.mid match {
      case a if (a < 200 || (a > 200 && a < 250)) => PolyTools.calculate(List(0.19, 0.2, 0.22, 0.27, 0.32, 0.54), v)
      case a if (a > 250 && a < 300) => PolyTools.calculate(List(0.26, 0.28, 0.32, 0.39, 0.45, 0.67), v)
      case a if (a > 300 && a < 350) => PolyTools.calculate(List(0.35, 0.37, 0.41, 0.5, 0.58, 0.87), v)
      case a if (a > 350 && a < 430) => PolyTools.calculate(List(0.45, 0.46, 0.53, 0.64, 0.73, 1), v)
      case a if (a > 430 && a < 470) => PolyTools.calculate(List(0.53, 0.57, 0.63, 0.78, 0.91, 1), v)
      case a if (a > 470 && a < 510) => PolyTools.calculate(List(0.63, 0.7, 0.78, 0.98, 1, 1), v)
      case a if (a > 510 && a < 600) => PolyTools.calculate(List(0.71, 0.9, 1, 1, 1, 1), v)
      case a if (a > 600) => PolyTools.calculate(List(0.8, 0.9, 1, 1, 1, 1), v)
    }, 1)
  }

  def getKHBet(v: Double, psiBA: Double, schemeType: Int, matrow: StandardParameters.MaterialTableRow): Double = {
    1 + (getKHBet0(psiBA, schemeType, matrow) - 1) * getKHW(v, matrow)
  }

  def getKHAlph(v: Double, matrow1: StandardParameters.MaterialTableRow,
                matrow2: StandardParameters.MaterialTableRow,
                accuracyGrade: Int,
                bevel: Boolean = false,
               ): Double = {
    if (!bevel) {
      1
    } else {
      val A = (matrow1, matrow2) match {
        case (a, b) if (a.outsideHB.mid > 350 && a.outsideHB.mid > 350) => 0.12
        case (a, b) if (a.outsideHB.mid <= 350 && a.outsideHB.mid <= 350) => 0.6
        case _ => 0.6
      }
      1 + A * (accuracyGrade - 5)
    }
  }

  /**
   *
   * @param matrow1    параметр шестерни
   * @param matrow2    параметр колеса
   * @param u          передаточное отношение от шестерни к колесу
   * @param T1         момент либо на центральном колесе / шестерне
   * @param n1         частота вращения центральной
   * @param n2         частота вращения сателитной
   * @param beta       угол бета, в начале расчета считается нулем
   * @param satellites количество сателитов, по умолчанию 3
   * @param inner      сигнал о внутреннем зацеплении
   * @param schemeType тип схема (консольная, либо межопорная и так даллее)
   * @param bevel      (сигнал о косозубости)
   * @return
   */
  def getAW(matrow1: StandardParameters.MaterialTableRow, matrow2: StandardParameters.MaterialTableRow, u: Double,
            T1: Double, n1: Double, n2: Double, satellites: Int, inner: Boolean = false,
            schemeType: Int = 1,
            beta: Double = 0 ,
            bevel: Boolean = false): Double = {
    //n1 -  на центральном
    val vel = getVelocity(matrow1, matrow2, u, T1, n1)
    val accuracyGrade = getAccuracyGrade(vel)
    val psiBA = getPsiBA(matrow1, matrow2)
    val KHBet = getKHBet(vel, psiBA, schemeType, matrow1)
    val KHAlph = getKHAlph(vel, matrow1, matrow2, accuracyGrade.toInt, bevel)
    val KHV = getKHV(matrow2, accuracyGrade.toInt, vel, bevel)
    val KH = KHV * KHAlph * KHBet
    val DSigH = SigH.fullFindDSigH(matrow1, matrow2, n1, n2, vel, if (inner) beta else 0)
    println(s"${CylindricGearTransmissionsCalculation.getClass.getName}: KH: ${KH}")
    450 * math.abs(u + getSign(inner) * 1) * math.pow((KH * T1 * getKW()) / (psiBA * u * satellites * DSigH * DSigH), 1 / 3.toFloat)
  }

  def getDSigH(matrow1: StandardParameters.MaterialTableRow, matrow2: StandardParameters.MaterialTableRow, u: Double,
               T1: Double, n1: Double, n2: Double, satellites: Int, inner: Boolean = false,
               schemeType: Int = 1,
               beta: Double = 0 ,
               bevel: Boolean = false): Double = {
    val vel = getVelocity(matrow1, matrow2, u, T1, n1)
    SigH.fullFindDSigH(matrow1, matrow2, n1, n2, vel, if (inner) beta else 0)
  }

  def findM(aw: Double, z_summ: Int): Double = {
    val pre_m = 2 * aw / z_summ
    StandardParameters.findNearestWithRound(StandardParameters.MS, pre_m)
  }

  def findAWbyM(m: Double, z_summ: Int) : Double = {
    z_summ * m / 2.toDouble
  }
}

object PolyTools {
  def calculate(func_coefs: List[Double], argument: Double): Double = {
    func_coefs.reverse.zipWithIndex.map(arg => math.pow(argument, arg._2) * arg._1).foldLeft(0.0)(_ + _)
  }
}
