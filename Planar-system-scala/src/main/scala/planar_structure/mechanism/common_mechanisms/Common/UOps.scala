package planar_structure.mechanism.common_mechanisms.Common

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.types.CarrierPosition
import planar_structure.subroutines.ChangeableParameters

abstract class CarrierDependent(partial : PartialWheelCalculator){
  val carrierPosition: CarrierPosition

  def sign : Double = partial.sign
  /**
   *
   * @param targetU целевое передаточное отношение
   * @return передаточное отношение при остановленном водиле
   */
  def U_direct_H(targetU: Double): Double

  /**
   *
   * @param u_directH передаточное отношение от первого звена к последнему при остановленном водиле
   * @return целевое передаточное отношение
   */
  def findTargetU(u_directH: Double): Double

  def findUdH(listBuffer: List[Int]): Double = {
    listBuffer.length match {
      case 4 => sign * listBuffer(1) / listBuffer(0).toFloat * listBuffer(3) / listBuffer(2).toFloat
      case 3 => sign * listBuffer(2) / listBuffer(0)
    }
  }
}

trait UDependent extends NumPy {
  val carrierDelegate : CarrierDependent
  def sign: Double

  def getInners: List[Boolean]

  def getTargetRights: List[Boolean]

  def getShortInners: List[Boolean] = {
    val inners = getInners
    List(inners(0) || inners(1), inners(2) || inners(3))
  }

  def z_sum1(z: List[Int]): Int

  def totalShift1(x: List[Double]): Double

  def z_sum2(z: List[Int]): Int

  def totalShift2(x: List[Double]): Double

  def maxSupposedZSum(z : List[Int]) : Double = {
    z.length match {
      case 4 =>
        if (z(1) > z(2)) z_sum1(z) else z_sum2(z)
      case 3 =>
        z_sum1(z)
    }
  }

  def maxSupposedSatelliteGear(z : List[Int]) : Double

  //обёртки под функции делегат
  def findUdH(listBuffer: List[Int]) : Double = {
    carrierDelegate.findUdH(listBuffer)
  }

  def findTargetU(u_directH: Double): Double = {
    carrierDelegate.findTargetU(u_directH)
  }

  def U_direct_H(targetU: Double): Double ={
    carrierDelegate.U_direct_H(targetU)
  }

}


trait UOps extends UDependent {
  def findUdirWithShift(z1: Int, z2: Int, cbeta: Double, calphat: Double, calphaw: Double): Double = {
    (z2 / z1.toDouble - 1) / (1 / cbeta * (calphat / calphaw - 1) + 1) + 1
  }

  def uCheckBoolean(listBuffer: List[Int], shifts: List[Double], betas: List[Double], targetU: Double, accuracy: Double): Boolean = {
    val u = findU(listBuffer, shifts, betas)
    checkPercent(u, targetU) <= accuracy * 100
  }

  def uCheckPercent(mech: Mechanism, targetU: Double): Double = {
    val u = findU(mech)
    checkPercent(u, targetU)
  }

  def uCheckPercent(listBuffer: List[Int], shifts: List[Double], betas: List[Double], targetU: Double): Double = {
    val u = findU(listBuffer, shifts, betas) //findTargetU(findUdH(listBuffer))
    checkPercent(u, targetU)
  }

  //u check
  def uCheckBooleanRaw(listBuffer: List[Int], targetU: Double, accuracy: Double = 0.05): Boolean = {
    val currentU = findU(listBuffer, listBuffer.map(_ => 0), if (listBuffer.length == 4) List(0, 0) else List(0)) //findTargetU(findUdH(listBuffer))
    if (checkPercent(currentU, targetU) <= accuracy * 100) {
      //println(s"\t\t$currentU")
      true
    } else false
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
}


trait AWOps extends UDependent {
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
}

trait AlignmentOps extends UDependent with AWOps {
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
        max_d1 = (aw_ + dsata * 0.5) * 2
      }
      else {
        val aw_ = aw(z_sum, shift_sum, beta, m) //findDa(z(0),m, x(0),z_sum, shift_sum,beta,ha,c,false)
        val dsata = findDa(z(0), m, x(0), z_sum, shift_sum, beta, ha, c, false)
        max_d1 = (aw_ + dsata * 0.5) * 2
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
    if (inner) {
      m * z - 2 * (ha - x - 0.2) * m
    } else {
      m * z + 2 * (ha + x) * m
    }
  }
}