package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.{CarrierOutput, ExternalInternal}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object B_AH_B_WheelCalculator extends WheelCalculator {
  override def U_direct_H(targetU: Float): Float = {
    1 - targetU
  }

  override def findUdH(listBuffer: ListBuffer[Int]): Double = {
    -listBuffer(1) / listBuffer(0).toFloat * listBuffer(3) / listBuffer(2).toFloat
  }

  override def assemblyCheck(wheelNumbers: ListBuffer[Int], satellites: Int = 3): Boolean = {
    if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(3), satellites)) true else false
  }

  override def unaccurateAlignment(wheelNumbers: ListBuffer[Int], gears_accuracy: Int): Boolean = {
    if (math.abs(wheelNumbers(0) + wheelNumbers(1) + wheelNumbers(2) - wheelNumbers(3)) <= gears_accuracy) true else false
  }

  override def neighborhoodCheck(wheelNumbers: ListBuffer[Int], satellites: Int): Boolean = {
    if ((wheelNumbers(0) + wheelNumbers(1)) * math.sin(math.Pi / satellites) > wheelNumbers(1) + 2) true else false
  }

  override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[ListBuffer[Int]] = {
    def find_i_g(za: Int, U: Double, zb_max: Int): ListBuffer[Double] = {
      var i_g_min = za * U / zb_max.toDouble
      while (za * U / i_g_min > zb_max) {
        i_g_min += 0.01
      }
      arange(i_g_min, 3.0, 0.001)
    }

    /**
     * finds pairs of i_f and i_g
     *
     * @param ig
     * @param U
     * @param za
     * @param if_max
     * @param gearsAccuracy
     * @return
     */
    def find_ig_if(ig: ListBuffer[Double],
                   U: Double,
                   za: Int,
                   if_max: Double,
                   gearsAccuracy: Int): ArrayBuffer[(Double, Double)] = {
      val varis = ArrayBuffer.empty[(Double, Double)]
      var if_ = 0.0
      for (i <- ig) {
        if_ = 1 / (gearsAccuracy / za - 1 + U / i) * (U / i - 1) * (1 + i) - 2
        for (if_next <- 0 until 5) {
          if (if_ + if_next * 0.01 <= if_max) {
            if_ = if_ + if_next * 0.01
            varis.addOne((i, if_))
          }
        }
      }
      varis
    }

    def findZ(i_g: Double, i_f: Double, z_a: Int, U: Double): ListBuffer[Int] = {
      val z_b = math.round(z_a * U / i_g).toInt
      val z_f = math.round((z_b - z_a) / (2 + i_f)).toInt
      val z_g = math.round(z_f * i_g).toInt
      (z_a, z_b, z_f, z_g)
      ListBuffer(z_a, z_g, z_f, z_b)
    }

    def findVariants(U: Double, za_max: Int = 24, zb_max: Int = 230, gears_accuracy: Int = 2, U_accuracy: Double = 0.05)
    : ListBuffer[ListBuffer[Int]] = {
      val acceptable_variants = ListBuffer.empty[ListBuffer[Int]]
      //находим коэффициенты всякие
      val i_g_if = find_ig_if(ig = find_i_g(za = 24, U = U, zb_max = zb_max),
        U = U, za = za_max, if_max = 2, gearsAccuracy = 2)
      var z_s: ListBuffer[Int] = null
      for (i <- i_g_if) {
        z_s = findZ(i_g = i._1, i_f = i._2, z_a = za_max, U = U)
        //теперб проверяем всякие условия
        if (assemblyCheck(z_s, satellites)) {
          if (neighborhoodCheck(z_s, satellites)) {
            if (unaccurateAlignment(z_s, gear_accuracy)) {
              if (uCheck(z_s,targetU, accuracyU))
                acceptable_variants.addOne(z_s)
            }
          }
        }
      }
      acceptable_variants
    }

    findVariants(U = math.abs(targetU - 1), U_accuracy = accuracyU)
  }

  override def accurateAlignment(z: IndexedSeq[Int])(alpha_t: Double): List[ShiftedWheel] = {
    null //TODO
  }

  override def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[B_AH_B_WheelCalculator.WithShiftedWheels] = null

  override def findTargetU(u_directH: Double): Double = {
    1 - u_directH
  }

  override def carrierFrequency(inputFreq: Double, wheelList: ListBuffer[Int], kpd: Double): Double = {
    inputFreq / (1 + wheelList(3) * wheelList(1) / wheelList(0).toFloat / wheelList(2).toFloat)
  }
}