package planar_structure.mechanism.common_mechanisms.Common

import planar_structure.mechanism.process.argument.AnalysisArgs
import planar_structure.mechanism.process.report.KinematicAnalysisReport
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}
import planar_structure.subroutines.ChangeableParameters


/**
 * must contain all necessary functions for analysis reports
 */
trait PartialWheelCalculator extends UOps with AlignmentOps with AWOps {
  def carrierPosition : CarrierPosition = carrierDelegate.carrierPosition
  val mechanismType: MechanismType

  //assembly
  def assemblyCheck(wheelNumbers: List[Int], satellites: Int = 3): Boolean = {
    wheelNumbers.length match {
      case 4 =>
        if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(3), satellites)) true else false
      case 3 =>
        if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(2), satellites)) true else false
    }
  }

  //neighborhood
  def neighborhoodCheck(wheelNumbers: List[Int], satellites: Int): Boolean = {
    if (satellites == 1) {
      true
    } else
    //в классе механизмов с для соседства расстояние между соседними сателлитами должно быть больше диаметра сателлита
    if (maxSupposedZSum(wheelNumbers) * math.sin(math.Pi / satellites) > maxSupposedSatelliteGear(wheelNumbers) + 2) true
    else false
  }

  def getAnalysisReport(args: AnalysisArgs): KinematicAnalysisReport = {
    getAnalysisReport(args.z, args.x, args.betas,
      args.modules, args.alpha, args.satellites, getInners, getTargetRights)
  }

  def getAnalysisReport(z: List[Int], x: List[Double],
                        betas: List[Double], modules: List[Double], alphas: List[Double],
                        satellites: Int,
                        inner: List[Boolean], //<-- зависимый от типа механизма параметр
                        targetIsRight: List[Boolean] //<-- зависимый от типа механизма параметр
                       ): KinematicAnalysisReport = {
    if (!(z.length == 4 || z.length == 3)) throw new IllegalArgumentException("can't have z length different from 3 or 4")
    z.length match {
      case 4 =>
        val betasCorrected = List(betas(0), betas(2))
        val modulesCorrected = List(modules(0), modules(2))

        val u = findU(z, x, betasCorrected)
        val aw2 = aw(z_sum2(z), totalShift2(x), betasCorrected(1), modulesCorrected(1))
        val accAw2 = checkPercent(aw2, aw(z_sum1(z), totalShift1(x), betasCorrected(0), modulesCorrected(0)))
        val maxSize = findMaxDiameter(z, x, List(z_sum1(z), z_sum2(z)), List(totalShift1(x), totalShift2(x)),
          betasCorrected, modulesCorrected, getShortInners, targetIsRight)
        val neihborhoodCheck = neighborhoodCheck(z, satellites)
        val assembly = assemblyCheck(z, satellites)
        val pruningCheck = pruningList(z, x, betasCorrected, ha = ChangeableParameters.HA, alphas
          , inner)
        KinematicAnalysisReport(u, aw2, accAw2, maxSize, neihborhoodCheck, assembly, pruningCheck)
      case 3 =>
        val betasCorrected = List(betas(0))
        val modulesCorrected = List(modules(0))

        val u = findU(z, x, betasCorrected)
        val aw2 = aw(z_sum2(z), totalShift2(x), betasCorrected(0), modulesCorrected(0))
        val accAw2 = checkPercent(aw2, aw(z_sum1(z), totalShift1(x), betasCorrected(0), modulesCorrected(0)))
        val maxSize = findMaxDiameter(z, x, List(z_sum1(z), z_sum2(z)), List(totalShift1(x), totalShift2(x)),
          betas, modulesCorrected, getShortInners, targetIsRight)
        val neihborhoodCheck = neighborhoodCheck(z, satellites)
        val assembly = assemblyCheck(z, satellites)
        val pruningCheck = pruningList(z, x, betasCorrected, ha = ChangeableParameters.HA, alphas
          , inner)
        KinematicAnalysisReport(u, aw2, accAw2, maxSize, neihborhoodCheck, assembly, pruningCheck)
    }
  }
}
