package planar_structure.mechanism.process.report

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.MECHANISM_FULL_CLASSIFIER

import scala.collection.mutable.ListBuffer

/**
 * @param sorted_mechanism contains synthesized mechanisms, sorted by some parameter
 *                         by default - its size, so the first one is the smallest mechanism
 */
case class KinematicSynthesisReport(sorted_mechanism:  Array[Mechanism])



case class SynthesizedMechanisms(sorted_mechanisms : ListBuffer[Mechanism],
                                 minimalSize : Double,
                                 mechanismAmount : Int = 0,
                                 mechClassifier : MECHANISM_FULL_CLASSIFIER = null,
                                 additionalInfo : ListBuffer[AdditionalInfoSynthesized],
                                 u_target : Double)

case class AdditionalInfoSynthesized(allowedTension: Tension, realTension : Tension,
                                     resU : Double,
                                     uAccuracy : Double,
                                     aw2 : Double,
                                     alignmentAccuracy: Double,
                                     maximumSize : Double)

case class Tension(sigma_h : Double, sigma_f : Double)
