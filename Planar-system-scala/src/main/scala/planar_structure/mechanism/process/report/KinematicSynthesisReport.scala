package planar_structure.mechanism.process.report

import planar_structure.mechanism.Mechanism

/**
 * @param sorted_mechanism contains synthesized mechanisms, sorted by some parameter
 *                         by default - its size, so the first one is the smallest mechanism
 */
case class KinematicSynthesisReport(sorted_mechanism:  Array[Mechanism])



case class SynthesizedMechanisms(sorted_mechanisms : List[Mechanism], mechanismAmount : Int = 0, minimalSize : Int = 0)