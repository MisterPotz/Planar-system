package planar_interface.model

import java.util.function.DoubleToLongFunction

import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.MECHANISM_FULL_CLASSIFIER
import planar_structure.mechanism.mech2kh.{Mechanism2KH, WheelInfo}
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.{CarrierOutput, ExternalInternal, MechanismType}

import scala.collection.immutable.NumericRange
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * делегировать задачу расчет передаточ
 */
object MechanismSynthesizer {
  case class WheelNumberArgs(mechanismType: MECHANISM_FULL_CLASSIFIER,
                             targetU : Double,
                             accuracy : Double,
                             satellites : Int,
                            )

  def sortedByMin(synthesizedMechanisms: SynthesizedMechanisms) : SynthesizedMechanisms = {
    val arr = synthesizedMechanisms.sorted_mechanisms
    SynthesizedMechanisms(arr.sortBy(mech => {
      val s = mech.getGears
      s(0).holder.z + s(1).holder.z
    }))
  }
  def minimalSize(synthesizedMechanisms: SynthesizedMechanisms) : Int = {
    val arr = synthesizedMechanisms.sorted_mechanisms
    val min = arr.minBy(mech => {
      val s = mech.getGears
      s(0).holder.z + s(1).holder.z
    })
    val gears = min.getGears
    gears(0).holder.z + gears(1).holder.z
  }
  def synthesizeByWheelNumber(wheelNumberArgs: WheelNumberArgs) : SynthesizedMechanisms = {
    var listBuff = ListBuffer.empty[Mechanism]
    var _1 : WheelInfo = null
    var _2 : WheelInfo = null
    var _3 : WheelInfo = null
    var _4 : WheelInfo  = null
    //делаем массив по i-
    val variants = B_AH_B.findVariants(U = math.abs(wheelNumberArgs.targetU - 1), U_accuracy = wheelNumberArgs.accuracy)
    val mechs = variants.foreach(gears => {
      _1 = WheelInfo(gears._1.toShort)
      _4 = WheelInfo(gears._2.toShort)
      _3 = WheelInfo(gears._3.toShort)
      _2 = WheelInfo(gears._4.toShort)
      listBuff.addOne(Mechanism2KH(ExternalInternal, CarrierOutput, List(_1,_2,_3,_4),wheelNumberArgs.satellites.toByte))
    })
    SynthesizedMechanisms(listBuff.toList)
  }

}

object B_AH_B{
  def find_i_g(za : Int, U : Double, zb_max : Int): ArrayBuffer[Double]= {
    var i_g_min = za*U/zb_max.toDouble
    while (za * U  / i_g_min > zb_max) {
      i_g_min += 0.01
    }
    arange(i_g_min,3.0,0.001)
  }
  def arange( start : Double, end: Double, step: Double) : ArrayBuffer[Double] = {
    var curr = start
    val arrayBuf = ArrayBuffer.empty[Double]
    while (curr <= end){
      arrayBuf.addOne(curr)
      curr += step
    }
    arrayBuf
  }

  /**
   * finds pairs of i_f and i_g
   * @param ig
   * @param U
   * @param za
   * @param if_max
   * @param gearsAccuracy
   * @return
   */
  def find_ig_if(ig : ArrayBuffer[Double],
                 U : Double,
                 za : Int ,
                 if_max: Double,
                 gearsAccuracy: Int): ArrayBuffer[(Double,Double)] = {
    val varis = ArrayBuffer.empty[(Double,Double)]
    var if_ = 0.0
    for (i <- ig) {
      if_ = 1 / (gearsAccuracy / za - 1 + U / i) * (U / i - 1) * (1 + i) - 2
      for (if_next <- 0 until 5){
        if (if_ + if_next * 0.01 <= if_max){
          if_ = if_ + if_next * 0.01
          varis.addOne((i, if_))
        }
      }
    }
    varis
  }
  def findZ(i_g: Double, i_f: Double, z_a: Int, U: Double): (Int,Int,Int,Int) ={
    val z_b = (z_a * U / i_g).round.toInt
    val z_f = ((z_b-z_a)/(2+i_f)).round.toInt
    val z_g = (z_f*i_g).round.toInt
    (z_a,z_b,z_f,z_g)
  }
  def isU(Uf: Double, U: Double, accuracy: Double) : Boolean = {
    if (U * (1-accuracy) <= Uf && U * (1+accuracy) >= Uf){
      true
    } else false
  }
  def alignment(v: (Int,Int,Int,Int), gears_accuracy: Int): Boolean = {
    if (math.abs(v._1 + v._3 + v._4 - v._2) <= gears_accuracy+0.001){
      true
    } else false
  }
  def checkGoodness(v: (Int,Int,Int,Int), U: Double, gears_accuracy : Int = 2, U_accuracy: Double = 0.05): Boolean = {
    if (isU(v._2/ v._1*v._4/v._3, U, U_accuracy)) {
      if (alignment(v,gears_accuracy = gears_accuracy))
        true
      else
        false
    } else
      false
  }
  def findVariants(U: Double, za_max : Int = 24, zb_max : Int = 230, gears_accuracy : Int = 2, U_accuracy: Double = 0.05)
  : ListBuffer[(Int, Int, Int, Int)] = {
    val acceptable_variants = ListBuffer.empty[(Int, Int, Int, Int)]
    val i_g_if = find_ig_if(ig = find_i_g(za = 24, U = U,zb_max = zb_max),
      U = U, za = za_max, if_max = 2, gearsAccuracy = 2)
    var z_s : (Int, Int, Int, Int) = (0,0,0,0)
    for (i <- i_g_if){
      z_s = findZ(i_g = i._1, i_f = i._2, z_a = za_max, U = U)
      if (checkGoodness(z_s,U = U,gears_accuracy = gears_accuracy)){
        acceptable_variants.addOne(z_s)
      }
    }
    acceptable_variants
  }
}

object MechSynthesizerTester extends App{
  var some = MechanismSynthesizer.synthesizeByWheelNumber(MechanismSynthesizer.WheelNumberArgs(planar_structure.mechanism.common_mechanisms.B_AH_B,
    10,0.05, 3))
  some = MechanismSynthesizer.sortedByMin(some)
  println(some.sorted_mechanisms.length)
  val s = some.sorted_mechanisms(0)
  println(s"${s.getGears.map{_.holder.z}}")
}