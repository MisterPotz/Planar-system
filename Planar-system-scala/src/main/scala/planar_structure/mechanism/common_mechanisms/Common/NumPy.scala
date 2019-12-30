package planar_structure.mechanism.common_mechanisms.Common

import planar_structure.mechanism.raw_algorithms.GearObjectedConversions
import planar_structure.subroutines.ChangeableParameters

import scala.collection.mutable.ListBuffer

trait NumPy extends GearObjectedConversions {
  def checkPercent(real: Double, target: Double): Double = {
    math.abs(math.abs(real / target) - 1) * 100
  }

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

  final def inv(angle: Double): Double = {
    math.tan(angle) - angle
  }

  //hta
  def hta(ha: Float = ChangeableParameters.HA, beta: Float = ChangeableParameters.BETFS): Double = ha / math.cos(beta).toFloat

  //xt
  def xt(x: Double)(beta: Float = ChangeableParameters.BETFS): Double = x / math.cos(beta).toFloat

  //alpha_t
  def alpha_t(alpha: Float = ChangeableParameters.ALF, beta: Float = ChangeableParameters.BETFS): Double
  = math.atan(math.tan(alpha) / math.cos(beta))

  //min x to get rid of pruning
  def xmin(z: Int, beta: Double = ChangeableParameters.BETFS, ha: Double = ChangeableParameters.HA, alpha_t_ : Double = alpha_t()): Double = {
    ha - z * math.pow(math.sin(alpha_t_), 2) / 2 / math.cos(beta)
  }

  //pruning on separate wheel
  def pruning(z: Int, xt: Double, ha: Double, alpha_t: Double): Boolean = {
    val z_min = (2 * (ha - xt) / math.pow(math.sin(alpha_t), 2.0)).floor
    if (z_min > z) false else true
  }

  def findAwt(shift_sum: Double, z_sum: Int, alpha_t: Double): Double = {
    ((2 * (shift_sum) * math.tan(alpha_t) / (z_sum).toFloat) + inv(alpha_t)).invToRad
  }

  def aw(z_summ: Int, total_shifts: Double, betas: Double, m: Double): Double = {
    val alphat1 = alpha_t(ChangeableParameters.ALF, betas.toFloat)
    //TODO здесь нашел сразу косинус а не норм значение
    val сalphaw1 = math.cos(findAwt(total_shifts, z_summ, alphat1))
    val ans = z_summ * m * math.cos(ChangeableParameters.ALF) / 2 / math.cos(betas) / (сalphaw1)
    ans
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
  }

  protected def isNotSimpleNumber(i: Int): Boolean = {
    if (i % 3 == 0 || i % 5 == 0 || i % 7 == 0 || i % 9 == 0) {
      true
    } else false
  }

  //pruning for internal wheels
  def pruningForInternal(x: Double): Boolean = if (x < 0) true else false

  protected def canHaveSatellites(wheelNumber: Int, satellites: Int = 3): Boolean = {
    if (wheelNumber % satellites == 0) true else false
  }
}
