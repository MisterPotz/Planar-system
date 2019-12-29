package planar_structure.mechanism.common_mechanisms.Common

import planar_structure.mechanism.common_mechanisms.B_AH_B_WheelCalculator.alpha_t
import planar_structure.mechanism.raw_algorithms.GearObjectedConversions
import planar_structure.subroutines.ChangeableParameters
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.argument.AnalysisArgs
import planar_structure.mechanism.process.report.KinematicAnalysisReport
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

import scala.collection.mutable.ListBuffer




trait WheelCalculator extends GearObjectedConversions {
  val mechanismType: MechanismType
  val carrierPosition: CarrierPosition

  final def inv(angle: Double): Double = {
    math.tan(angle) - angle
  }

  def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double = 1): Double

  def arange(start: Double, end: Double, step: Double): ListBuffer[Double] = {
    var curr = start
    val arrayBuf = ListBuffer.empty[Double]
    while (math.abs(curr) < math.abs(end)) {
      arrayBuf.addOne(curr)
      curr += step
    }
    arrayBuf
  }

  def linspace(start: Double, end: Double, parts: Int): ListBuffer[Double] = {
    val step = (end - start) / parts.toFloat
    arange(start, end, step)
  }

  protected def isNotSimpleNumber(i: Int): Boolean = {
    if (i % 3 == 0 || i % 5 == 0 || i % 7 == 0 || i % 9 == 0) {
      true
    } else false
  }

  protected def canHaveSatellites(wheelNumber: Int, satellites: Int = 3): Boolean = {
    if (wheelNumber % satellites == 0) true else false
  }

  case class ShiftedWheel(z: Int, shift: Double = 0)

  case class WithShiftedWheels(wheels: List[ShiftedWheel])

  def U_direct_H(targetU: Float): Float

  def findTargetU(u_directH: Double): Double

  def findUdirWithShift(z1: Int, z2: Int, cbeta: Double, calphat: Double, calphaw: Double): Double = {
    (z2 / z1.toDouble - 1) / (1 / cbeta * (calphat / calphaw - 1) + 1) + 1
  }

  def findAwt(shift_sum: Double, z_sum: Int, alpha_t: Double): Double = {
    ((2 * (shift_sum) * math.tan(alpha_t) / (z_sum).toFloat) + inv(alpha_t)).invToRad
  }

  def sign: Double

  def uCheck(listBuffer: List[Int], shifts: List[Double], betas: List[Double], targetU: Double, accuracy: Double): Boolean = {
    val u = findU(listBuffer, shifts, betas)
    checkPercent(u, targetU) <= accuracy * 100
  }

  def uCheckMeaning(listBuffer: List[Int], shifts: List[Double], betas: List[Double], targetU: Double, accuracy: Double): Double = {
    findU(listBuffer, shifts, betas)
  }

  def uCheckPercent(mech: Mechanism, targetU: Double): Double = {
    val u = findU(mech)
    checkPercent(u, targetU)
  }

  //TODO ну и какого фига здесь рядом похожие методы но они нифига не одинаковый резалт имеют?
  def uCheckPercent(listBuffer: List[Int], targetU: Double, accuracy: Double = 0.05): Double = {
    val u = findU(listBuffer, listBuffer.map(_ => 0), if (listBuffer.length == 4) List(0, 0) else List(0)) //findTargetU(findUdH(listBuffer))
    checkPercent(u, targetU)
  }

  //u check
  def uCheck(listBuffer: List[Int], targetU: Double, accuracy: Double = 0.05): Boolean = {
    val currentU = findU(listBuffer, listBuffer.map(_ => 0), if (listBuffer.length == 4) List(0, 0) else List(0)) //findTargetU(findUdH(listBuffer))
    if (checkPercent(currentU, targetU) <= accuracy * 100) {
      println(s"\t\t$currentU")
      true
    } else false
  }

  def checkPercent(real: Double, target: Double): Double = {
    math.abs(math.abs(real / target) - 1) * 100
  }

  def findU(z: List[Int], x: List[Double], betas: List[Double]): Double = {
    if (!(betas.length == 2 || betas.length == 1)) throw new IllegalArgumentException("can't have betas with size not 2")
    //if (modules.length != 2) throw  new IllegalArgumentException("can't have modules with size not 2")
    if (z.length == 4) {
      val alphat1 = alpha_t(ChangeableParameters.ALF, betas(0).toFloat)
      val u1 = findUdirWithShift(z(0), z(1), math.cos(betas(0)),
        math.cos(alphat1), math.cos(findAwt(totalShift1(x), z_sum1(z), alphat1)))
      if (u1 < 0) println("NEGATIVE U1")
      val alphat2 = alpha_t(ChangeableParameters.ALF, betas(1).toFloat)
      val u2 = findUdirWithShift(z(2), z(3), math.cos(betas(1)),
        math.cos(alphat2), math.cos(findAwt(totalShift2(x), z_sum2(z), alphat2)))
      if (u2 < 0) println("NEGATIVE U2")

      findTargetU(u1 * u2 * sign)
    } else {
      val alphat1 = alpha_t(ChangeableParameters.ALF, betas(0).toFloat)
      val u1 = findUdirWithShift(z(0), z(1), math.cos(betas(0)),
        math.cos(alphat1), math.cos(findAwt(totalShift1(x), z_sum1(z), alphat1)))
      if (u1 < 0) println("NEGATIVE U1")
      val alphat2 = alphat1
      val u2 = findUdirWithShift(z(1), z(2), math.cos(betas(0)),
        math.cos(alphat2), math.cos(findAwt(totalShift2(x), z_sum2(z), alphat2)))
      if (u2 < 0) println("NEGATIVE U2")
      findTargetU(u1 * u2 * sign)
    }
  }

  def findU(mech: Mechanism): Double = {
    val gears = mech.getGears
    val z = gears.map(_.holder.z)
    val shifts = gears.map(_.holder.x.toDouble)
    val betas = List(gears(0).holder.beta.toDouble, gears(2).holder.beta.toDouble)
    findU(z, shifts, betas)
  }

  def findUdH(listBuffer: List[Int]): Double = {
    listBuffer.length match {
      case 4 => sign * listBuffer(1) / listBuffer(0).toFloat * listBuffer(3) / listBuffer(2).toFloat
      case 3 => sign * listBuffer(2) / listBuffer(0)
    }

  }

  //assembly
  def assemblyCheck(wheelNumbers: List[Int], satellites: Int = 3): Boolean

  //unaccurate alignment
  def unaccurateAlignment(wheelNumbers: List[Int], gears_accuracy: Int = 2): Boolean

  //neighborhood
  def neighborhoodCheck(wheelNumbers: List[Int], satellites: Int): Boolean

  //hta
  def hta(ha: Float = ChangeableParameters.HA, beta: Float = ChangeableParameters.BETFS): Double = ha / math.cos(beta).toFloat

  //xt
  def xt(x: Double)(beta: Float = ChangeableParameters.BETFS): Double = x / math.cos(beta).toFloat

  //alpha_t
  def alpha_t(alpha: Float = ChangeableParameters.ALF, beta: Float = ChangeableParameters.BETFS): Double = math.atan(math.tan(alpha) / math.cos(beta))

  //pruning on separate wheel
  def pruning(z: Int, xt: Double, ha: Double, alpha_t: Double): Boolean = {
    val z_min = (2 * (ha - xt) / math.pow(math.sin(alpha_t), 2.0)).floor
    if (z_min > z) false else true
  }

  //min x to get rid of pruning
  def xmin(z: Int, beta: Double = ChangeableParameters.BETFS, ha: Double = ChangeableParameters.HA, alpha_t_ : Double = alpha_t()): Double = {
    ha - z * math.pow(math.sin(alpha_t_), 2) / 2 / math.cos(beta)
  }

  //pruning for internal wheels
  def pruningForInternal(x: Double): Boolean = if (x < 0) true else false

  //some initial set of variants
  def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[List[Int]]

  //accurate alignment fix, must return paired z and x of both wheels. if no adequate combination is found - returns some message
  def accurateAlignment(z: IndexedSeq[Int])(alpha_t_ : Double = alpha_t()): List[ShiftedWheel]

  def aw(z_summ: Int, total_shifts: Double, betas: Double, m: Double): Double = {
    val alphat1 = alpha_t(ChangeableParameters.ALF, betas.toFloat)
    //TODO здесь нашел сразу косинус а не норм значение
    val сalphaw1 = math.cos(findAwt(total_shifts, z_summ, alphat1))
    val ans = z_summ * m * math.cos(ChangeableParameters.ALF) / 2 / math.cos(betas) / (сalphaw1)
    ans
  }

  def awMech(mech: Mechanism, row: Int): Double = {
    val gears = mech.getGears
    val z = gears.map(_.holder.z)
    val shifts = gears.map(_.holder.x.toDouble)
    val betas = List(gears(0).holder.beta, gears(2).holder.beta.toDouble)
    val m = List(gears(0).holder.m, gears(2).holder.m.toDouble)
    gears.length match {
      case 4 =>
        if (row == 0) {
          aw(z_sum1(z), totalShift1(shifts), betas(0), m(0))
        } else {
          aw(z_sum2(z), totalShift2(shifts), betas(1), m(0))
        }
      case 3 =>
        if (row == 0) {
          aw(z_sum1(z), totalShift1(shifts), betas(0), m(0))
        } else {
          aw(z_sum2(z), totalShift2(shifts), betas(0), m(0))
        }
    }
  }

  //TODO нужно обернуть в каждрм классе для адекватного использования
  def findMaxDiameter(z: List[Int], x: List[Double], z_summ: List[Int],
                      total_shifts: List[Double], betas: List[Double], m: List[Double], inner: List[Boolean], inner_is_right: List[Boolean],
                      ha: Double = ChangeableParameters.HA, c: Double = ChangeableParameters.C): Double = {
    if (!(inner.length == 2)) throw new IllegalArgumentException("can't have inner with size not equal to 2")
    z.length match {
      case 4 =>
        //первая ступень
        val d_max1 = findMaxDStage(z.slice(0, 2), m(0), x.slice(0, 2), z_summ(0), total_shifts(0), betas(0), ha, c, inner(0), inner_is_right(0))
        //вторая ступень
        val d_max2 = findMaxDStage(z.slice(2, 4), m(1), x.slice(2, 4), z_summ(1), total_shifts(1), betas(1), ha, c, inner(1), inner_is_right(1))
        math.max(d_max1, d_max2)
      case 3 =>
        //первая ступень
        val d_max2 = findMaxDStage(z.slice(1, 3), m(0), x.slice(1, 3), z_summ(1), total_shifts(1), betas(0), ha, c, inner(1), inner_is_right(0))
        d_max2
    }
  }


  def findMaxDStage(z: List[Int], m: Double, x: List[Double], z_sum: Int, shift_sum: Double,
                    beta: Double,
                    ha: Double = ChangeableParameters.HA,
                    c: Double = ChangeableParameters.C,
                    inner: Boolean, target_is_right: Boolean): Double = {
    if (z.length != 2) throw new IllegalArgumentException("can't have z with size different from 2")
    var max_d1 = 0.0
    if (inner) {
      if (target_is_right) {
        max_d1 = findDf(z(1), m, x(1), ha, c, inner)
      }
      else {
        max_d1 = findDf(z(0), m, x(0), ha, c, inner)
      }
    } else {
      if (target_is_right) {
        val aw_ = aw(z_sum, shift_sum, beta, m) //findDa(z(0),m, x(0),z_sum, shift_sum,beta,ha,c,false)
        val dsata = findDa(z(1), m, x(1), z_sum, shift_sum, beta, ha, c, false)
        max_d1 = (aw_ + dsata) * 2
      }
      else {
        val aw_ = aw(z_sum, shift_sum, beta, m) //findDa(z(0),m, x(0),z_sum, shift_sum,beta,ha,c,false)
        val dsata = findDa(z(0), m, x(0), z_sum, shift_sum, beta, ha, c, false)
        max_d1 = (aw_ + dsata) * 2
      }
    }
    max_d1
  }

  def findDf(z: Int, m: Double, x: Double, ha: Double = ChangeableParameters.HA, c: Double = ChangeableParameters.C, inner: Boolean): Double = {
    if (inner) {
      m * z + 2 * (ha + c + x) * m
    } else {
      m * z - 2 * (ha + c - x) * m
    }
  }

  def findDa(z: Int, m: Double, x: Double, z_sum: Int, shift_sum: Double,
             beta: Double,
             ha: Double = ChangeableParameters.HA,
             c: Double = ChangeableParameters.C,
             inner: Boolean): Double = {
    /*val alpha_t_ = alpha_t(beta = beta.toFloat)
    val awt = findAwt(shift_sum,z_sum, alpha_t_)
    val y = z_sum / 2 / math.cos(beta) * (math.cos(alpha_t_) / math.cos(awt) - 1)
    val delta_y = z_sum - y*/
    if (inner) {
      m * z - 2 * (ha - x - 0.2) * m
    } else {
      m * z + 2 * (ha + x) * m
    }
  }

  def accurateAlignment(z: List[Int], x: List[Double], betas: List[Double], m: List[Double], accuracy: Double): Boolean = {
    if (!(z.length == 4 || z.length == 3)) throw new IllegalArgumentException("wrong z size")
    //TODO здесь нашел сразу косинус а не норм значение
    z.length match {
      case 4 =>
        val aw1 = aw(z_sum1(z.toList), totalShift1(x), betas(0), m(0))
        val aw2 = aw(z_sum2(z.toList), totalShift2(x), betas(1), m(1))
        if (checkPercent(aw1, aw2) <= accuracy * 100) {
          true
        } else false
      case 3 =>
        val aw1 = aw(z_sum1(z.toList), totalShift1(x), betas(0), m(0))
        val aw2 = aw(z_sum2(z.toList), totalShift2(x), betas(0), m(0))
        if (checkPercent(aw1, aw2) <= accuracy * 100) {
          true
        } else false
    }

  }

  def accurateAlignmentPercent(z: List[Int], x: List[Double], betas: List[Double], m: List[Double]): Double = {
    z.length match {
      case 4 =>
        val aw1 = aw(z_sum1(z), totalShift1(x), betas(0), m(0))
        val aw2 = aw(z_sum2(z), totalShift2(x), betas(1), m(1))
        checkPercent(aw1, aw2)
      case 3 =>
        val aw1 = aw(z_sum1(z), totalShift1(x), betas(0), m(0))
        val aw2 = aw(z_sum2(z), totalShift2(x), betas(0), m(0))
        checkPercent(aw1, aw2)
    }
  }

  def accurateAlignmentPercent(mechanism: Mechanism): Double = {
    val aw1 = awMech(mechanism, 0)
    val aw2 = awMech(mechanism, 1)
    checkPercent(aw1, aw2)
  }

  def z_sum1(z: List[Int]): Int

  def totalShift1(x: List[Double]): Double

  def z_sum2(z: List[Int]): Int

  def totalShift2(x: List[Double]): Double

  //final set of variants
  def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[WithShiftedWheels]

  import scala.collection.mutable.Set

  def neutralizeExtraVariants(variants: ListBuffer[List[Int]]): List[List[Int]] = {
    val set = Set.empty[List[Int]]
    variants.foreach(variant => set.addOne(variant.toList))
    set.toList
  }

  def pruningList(z: List[Int], x: List[Double],
                  betas: List[Double], ha: Double,
                  alpha: List[Double], inner: List[Boolean]): List[Boolean] = {
    if (!(inner.length == 3 || inner.length == 4)) throw new IllegalArgumentException("can't have inner with size different from 3 or 4")
    val innerCorrected: List[Boolean] = {
      z.length match {
        case 4 => z.zipWithIndex.map(z => {
          z._2 match {
            case 0 => inner(0)
            case a if (a == 1 || a == 2) => false
            case 3 => inner(3)
          }
        })
        case 3 => z.zipWithIndex.map(z => {
          z._2 match {
            case 0 => inner(0)
            case 1 => false
            case 2 => inner(2)
          }
        })
      }

    }
    Range(0, z.length).map { i =>
      if (innerCorrected(i)) {
        true
        //если какое-то колесо внутреннее, то будет ли действовать какое-то ограничение на это??
      } else
        pruning(z(i), xt(x(i))(betas(i % 2).toFloat), ha, alpha_t(alpha(i % 2).toFloat, betas(i % 2).toFloat))
    }.toList
  } //TODO зависит от типа колеса и механизма

  def getAnalysisReport(args: AnalysisArgs): KinematicAnalysisReport = {
    getAnalysisReport(args.z, args.x, args.betas,
      args.modules, args.alpha, args.satellites, getInners, getTargetRights)
  }

  def getInners: List[Boolean] //TODO

  def getTargetRights: List[Boolean]

  def getShortInners: List[Boolean] = {
    val inners = getInners
    List(inners(0) || inners(1), inners(2) || inners(3))
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
        val u = findU(z, x, betas)
        val aw2 = aw(z_sum2(z), totalShift2(x), betas(2), modules(2))
        val accAw2 = checkPercent(aw2, aw(z_sum1(z), totalShift1(x), betas(0), modules(0)))
        val maxSize = findMaxDiameter(z, x, List(z_sum1(z), z_sum2(z)), List(totalShift1(x), totalShift2(x)),
          List(betas(0), betas(2)), List(modules(0), modules(2)), getShortInners, targetIsRight)
        val neihborhoodCheck = neighborhoodCheck(z, satellites)
        val assembly = assemblyCheck(z, satellites)
        val pruningCheck = pruningList(z, x, betas, ha = ChangeableParameters.HA, alphas
          , inner)
        KinematicAnalysisReport(u, aw2, accAw2, maxSize, neihborhoodCheck, assembly, pruningCheck)
      case 3 =>
        val u = findU(z, x, betas)
        val aw2 = aw(z_sum2(z), totalShift2(x), betas(0), modules(0))
        val accAw2 = checkPercent(aw2, aw(z_sum1(z), totalShift1(x), betas(0), modules(0)))
        val maxSize = findMaxDiameter(z, x, List(z_sum1(z), z_sum2(z)), List(totalShift1(x), totalShift2(x)),
          List(betas(0)), List(modules(0)), getShortInners, targetIsRight)
        val neihborhoodCheck = neighborhoodCheck(z, satellites)
        val assembly = assemblyCheck(z, satellites)
        val pruningCheck = pruningList(z, x, betas, ha = ChangeableParameters.HA, alphas
          , inner)
        KinematicAnalysisReport(u, aw2, accAw2, maxSize, neihborhoodCheck, assembly, pruningCheck)
    }
  }
}



