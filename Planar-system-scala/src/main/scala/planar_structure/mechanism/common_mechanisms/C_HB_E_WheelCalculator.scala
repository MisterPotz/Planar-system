package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, WheelCalculator}
import planar_structure.mechanism.types.{CarrierInput, CarrierPosition, InternalInternal, MechanismType}
import planar_structure.subroutines.PolyTools

import scala.collection.mutable.ListBuffer

object C_HB_E_WheelCalculator extends WheelCalculator {
  override def carrierPosition: CarrierPosition = CarrierInput


  override def U_direct_H(targetU: Double): Double = {
    1 - 1 / targetU
  }

  override def findTargetU(u_directH: Double): Double = {
    if (u_directH < 0) println(s"LESS THAN ZERO: ${u_directH}")
    1 / math.abs(1 - 1 / math.abs(u_directH))
  }

  override def unaccurateAlignment(wheelNumbers: List[Int], gears_accuracy: Int): Boolean = {
    if (math.abs(wheelNumbers(0) - wheelNumbers(1) + wheelNumbers(2) - wheelNumbers(3)) <= gears_accuracy) true else false
  }

  override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[List[Int]] = {
    def poly_fit_if_max(u: Double): (Double) = PolyTools.calculate(List(-1.021e-08, 7.308e-06, -0.001703, 1.189), u)

    def poly_fit_if_min(u: Double): (Double) = PolyTools.calculate(List(-7.017e-09, 4.173e-06, -0.000772, 1.056), u)

    def poly_fit_ie_max(u: Double): (Double) = PolyTools.calculate(List(-3.533e-09, 3.262e-06, -0.0009062, 1.133), u)

    def poly_fit_ie_min(u: Double): (Double) = PolyTools.calculate(List(-1.345e-09, 7.364e-07, -9.345e-05, 1.007), u)

    val directU = math.abs(U_direct_H(targetU.toFloat))
    val if_max = poly_fit_if_max(targetU) + poly_fit_if_min(targetU) * 0.2 //были результаты и при 0.1
    val if_min = poly_fit_if_min(targetU) - poly_fit_if_min(targetU) * 0.2
    val ie_max = poly_fit_ie_max(targetU) + poly_fit_ie_min(targetU) * 0.2
    val ie_min = poly_fit_ie_min(targetU) - poly_fit_ie_min(targetU) * 0.2
    val if_ = linspace(if_min, if_max, 20)
    val if_ie = if_.map(f => (f, directU * f)).filter(fe => fe._2 >= ie_min && fe._2 <= ie_max)
    val zb = arange(50, 290, 1).filter(some => canHaveSatellites(some.toInt, satellites)).map(_.toInt)
    val zg = arange(15, 230, 1).map(_.toInt)
    val list_final = ListBuffer.empty[List[Int]]
    for (fe_ <- if_ie) {
      for (b <- zb) {
        val e = math.round(b * fe_._2).toInt
        for (g <- zg) {
          if (g < b - 8) {
            val f = math.round(g * fe_._1).toInt
            if (e - f > 8) {
              val list = List(e, f, g, b)
              if (unaccurateAlignment(list, gear_accuracy)) {
                if (uCheckBooleanRaw(list, targetU, accuracyU)) {
                  if (neighborhoodCheck(list, satellites))
                    list_final.addOne(list)
                }
              }
            }
          }
        }
      }
    }
    list_final.filter(list => uCheckBooleanRaw(list, targetU, accuracyU))
  }

  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = {
    inputFreq
  }

  override def sign: Double = 1

  override def z_sum1(z: List[Int]): Int = z(0) - z(1)

  override def totalShift1(x: List[Double]): Double = x(0) - x(1)

  override def z_sum2(z: List[Int]): Int = z(3) - z(2)

  override def totalShift2(x: List[Double]): Double = x(3) - x(2)

  override def getInners: List[Boolean] = List(true, false, false, true)

  override def getTargetRights: List[Boolean] = List(false, true)

  override val mechanismType: MechanismType = InternalInternal

  override def maxSupposedZSum(z: List[Int]): Double = z_sum1(z)

  override def maxSupposedSatelliteGear(z: List[Int]): Double = z(1)

  override val carrierDelegate: CarrierDependent = null
}
