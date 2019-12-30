package planar_structure.mechanism.common_mechanisms.Common

import scala.collection.mutable.ListBuffer

/**
 * contains functionality that is used in synthesis
 */
trait WheelCalculator extends PartialWheelCalculator {

  override def findUdH(listBuffer: List[Int]) : Double = {
    listBuffer.length match {
      case 4 => sign * listBuffer(1) / listBuffer(0).toFloat * listBuffer(3) / listBuffer(2).toFloat
      case 3 => sign * listBuffer(2) / listBuffer(0)
    }
  }

  def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double = 1): Double

  //unaccurate alignment
  def unaccurateAlignment(wheelNumbers: List[Int], gears_accuracy: Int = 2): Boolean

  //some initial set of variants
  def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[List[Int]]

  import scala.collection.mutable.Set

  def neutralizeExtraVariants(variants: ListBuffer[List[Int]]): List[List[Int]] = {
    val set = Set.empty[List[Int]]
    variants.foreach(variant => set.addOne(variant.toList))
    set.toList
  }
}



