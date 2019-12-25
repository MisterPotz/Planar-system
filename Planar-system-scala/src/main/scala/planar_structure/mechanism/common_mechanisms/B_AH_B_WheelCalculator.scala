package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.{CarrierOutput, ExternalInternal}
import planar_structure.subroutines.ChangeableParameters

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object B_AH_B_WheelCalculator extends WheelCalculator {
  override def U_direct_H(targetU: Float): Float = {
    targetU - 1
  }

  override def assemblyCheck(wheelNumbers: List[Int], satellites: Int = 3): Boolean = {
    if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(3), satellites)) true else false
  }

  override def unaccurateAlignment(wheelNumbers: List[Int], gears_accuracy: Int): Boolean = {
    if (math.abs(wheelNumbers(0) + wheelNumbers(1) + wheelNumbers(2) - wheelNumbers(3)) <= gears_accuracy) true else false
  }

  override def neighborhoodCheck(wheelNumbers: List[Int], satellites: Int): Boolean = {
    if ((wheelNumbers(0) + wheelNumbers(1)) * math.sin(math.Pi / satellites) > wheelNumbers(1) + 2) true else false
  }

  override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[List[Int]] = {
    def find_i_g(za: Int, U: Double, zb_max: Int): ListBuffer[Double] = {
      var i_g_min = za * U / zb_max.toDouble
      while (za * U / i_g_min > zb_max) {
        i_g_min += 0.01
      }
      arange(i_g_min, 3.0, 0.005)
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
    def find_ig_if(ig: List[Double],
                   U: Double, //direct
                   za: Int,
                   if_max: Double,
                   gearsAccuracy: Int): ListBuffer[(Double, Double)] = {
      val varis = ListBuffer.empty[(Double, Double)]
      var if_ = 0.0
      for (i <- ig) {
        //TODO надо упростить выражение здес
        for (f <- arange(0.01, if_max, 0.01)) {
          val target = (U / i - 1) * ((1 + i) / (2 + f) - 1)
          val assessment = gear_accuracy / za.toFloat
          if (math.abs(target) < assessment) {
            (varis.addOne((i, f)))
          }
        }
      }
      varis
    }

    def findZ(i_g: Double, i_f: Double, z_a: Int, U: Double): List[Int] = {
      val z_b = math.round(z_a * U / i_g).toInt
      val z_f = math.round((z_b - z_a) / (2 + i_f)).toInt
      val z_g = math.round(z_f * i_g).toInt
      (z_a, z_b, z_f, z_g)
      List(z_a, z_g, z_f, z_b)
    }

    def findVariants(U: Double, za_max: Int = 24, zb_max: Int = 230, gears_accuracy: Int, U_accuracy: Double = 0.05)
    : ListBuffer[List[Int]] = {
      val acceptable_variants = ListBuffer.empty[List[Int]]
      //находим коэффициенты всякие
      val i_g_if = find_ig_if(ig = find_i_g(za = za_max, U = U, zb_max = zb_max).toList,
        U = U, za = za_max, if_max = 2.5, gear_accuracy)
      var z_s: List[Int] = null
      for (i <- i_g_if) {
        z_s = findZ(i_g = i._1, i_f = i._2, z_a = za_max, U = U)
        //теперб проверяем всякие условия
        if (assemblyCheck(z_s, satellites)) {
          if (neighborhoodCheck(z_s, satellites)) {
            if (unaccurateAlignment(z_s, gear_accuracy)) {
              if (uCheck(z_s, targetU, accuracyU))
                acceptable_variants.addOne(z_s)
            }
          }
        }
      }
      acceptable_variants
    }

    findVariants(U = math.abs(targetU - 1), U_accuracy = accuracyU, gears_accuracy = gear_accuracy)
  }

  override def accurateAlignment(z: IndexedSeq[Int])(alpha_t: Double): List[ShiftedWheel] = {
    null //TODO
  }


  override def findTargetU(u_directH: Double): Double = {
    1 - u_directH
  }

  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = {
    inputFreq / (1 + wheelList(3) * wheelList(1) / wheelList(0).toFloat / wheelList(2).toFloat)
  }

  override def sign: Double = -1

  override def totalShift1(x: List[Double]): Double = x(0) + x(1)

  override def totalShift2(x: List[Double]): Double = x(3) - x(2)

  override def z_sum1(z: List[Int]): Int = z(0) + z(1)

  override def z_sum2(z: List[Int]): Int = z(3) - z(2)

  override def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[B_AH_B_WheelCalculator.WithShiftedWheels] = ???

  override def getInners: List[Boolean] = List(false, true)

  override def getTargetRights: List[Boolean] = List(true, true)
}