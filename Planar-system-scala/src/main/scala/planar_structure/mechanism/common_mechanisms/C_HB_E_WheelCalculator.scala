package planar_structure.mechanism.common_mechanisms

import planar_structure.subroutines.PolyTools

import scala.collection.mutable.ListBuffer

object C_HB_E_WheelCalculator extends WheelCalculator {
  override def U_direct_H(targetU: Float): Float = {
    1 - 1 / targetU
  }

  override def findUdH(listBuffer: ListBuffer[Int]): Double = {
    listBuffer(1) / listBuffer(0).toFloat * listBuffer(3) / listBuffer(2).toFloat
  }

  def findTargetU(u_directH: Double): Double = {
    1 / (1 - u_directH)
  }

  override def assemblyCheck(wheelNumbers: ListBuffer[Int], satellites: Int): Boolean = {
    if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(3), satellites)) true else false
  }

  override def unaccurateAlignment(wheelNumbers: ListBuffer[Int], gears_accuracy: Int): Boolean = {
    if (math.abs(wheelNumbers(0) - wheelNumbers(1) + wheelNumbers(2) - wheelNumbers(3)) <= gears_accuracy) true else false
  }

  override def neighborhoodCheck(wheelNumbers: ListBuffer[Int], satellites: Int): Boolean = {
    if ((wheelNumbers(0) + wheelNumbers(1)) * math.sin(math.Pi / satellites) > wheelNumbers(1) + 2) true else false
  }

  override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[ListBuffer[Int]] = {
    def poly_fit_if_max(u: Double): (Double) = PolyTools.calculate(List(-1.021e-08, 7.308e-06, -0.001703, 1.189), u)

    def poly_fit_if_min(u: Double): (Double) = PolyTools.calculate(List(-7.017e-09, 4.173e-06, -0.000772, 1.056), u)

    def poly_fit_ie_max(u: Double): (Double) = PolyTools.calculate(List(-3.533e-09, 3.262e-06, -0.0009062, 1.133), u)

    def poly_fit_ie_min(u: Double): (Double) = PolyTools.calculate(List(-1.345e-09, 7.364e-07, -9.345e-05, 1.007), u)

    val if_max = poly_fit_if_max(targetU)
    val if_min = poly_fit_if_min(targetU)
    val ie_max = poly_fit_ie_max(targetU)
    val ie_min = poly_fit_ie_min(targetU)
    val if_ = linspace(if_min, if_max, 5)
    val if_ie = if_.map(f => (f, targetU * f)).filter(fe => fe._2 >= ie_min && fe._2 <= ie_max)
    val zb = arange(40, 230, 1).filter(some => canHaveSatellites(some.toInt, satellites)).map(_.toInt)
    val zg = arange(15, 100, 1).map(_.toInt)
    val list_final = ListBuffer.empty[ListBuffer[Int]]
    for (fe_ <- if_ie) {
      for (b <- zb) {
        val e = math.round(b * fe_._2).toInt
        for (g <- zg) {
          if (g < b - 8) {
            val f = math.round(g * fe_._1).toInt
            val list = ListBuffer(b, f, g, e)
            if (unaccurateAlignment(list, gear_accuracy)) {
              if (uCheck(list, targetU, accuracyU)) {
                list_final.addOne(list)
              }
            }
          }
        }
      }
    }
    list_final
  }

  override def accurateAlignment(z: IndexedSeq[Int])(alpha_t: Double): List[ShiftedWheel] = null //TODO

  override def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[WithShiftedWheels] = null //TODO
  override def carrierFrequency(inputFreq: Double, wheelList: ListBuffer[Int], kpd: Double): Double = 0
}