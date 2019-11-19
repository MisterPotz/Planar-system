package planar_structure.mechanism.mech2kh.static_methods

import java.util.concurrent.ForkJoinPool

import planar_structure.mechanism.types.{External1, ExternalExternal, ExternalInternal, Internal1, InternalExternal, InternalInternal, MechanismType}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable.ParVector

//TODO does not take into account m
//TODO must calculate the number of gears for the 4th or 3d wheel
trait MechanismTypeDependentCalculations {
  def z3_Internal1(z_list: List[Short]) : Short = {
    (z_list(0) - z_list(1)).toShort
  }
  def z4_ExternalInternal(z_list: List[Short]) : Short= {
    (z_list(0) + z_list(1) + z_list(2)).toShort
  }
  def z4_InternalExternal(z_list: List[Short]) : Short= {
    (z_list(0) - z_list(1) - z_list(2)).toShort
  }
  def z4_ExternalExternal(z_list: List[Short]) : Short= {
    (z_list(0) + z_list(1) - z_list(2)).toShort
  }
  def z4_InternalInternal(z_list: List[Short]) : Short= {
    (z_list(0) - z_list(1) + z_list(2)).toShort
  }
  def z3_External1(z_list: List[Short]) : Short= {
    (z_list(0) + z_list(1)).toShort
  }
  def z_last(z_list: List[Short], mechanismType : MechanismType) : Short = {
    mechanismType match {
      case Internal1 => z3_Internal1(z_list)
      case ExternalExternal => z4_ExternalExternal(z_list)
      case ExternalInternal => z4_ExternalInternal(z_list)
      case InternalExternal => z4_InternalExternal(z_list)
      case InternalInternal => z4_InternalInternal(z_list)
      case External1 => z3_External1(z_list)
    }
  }
  //-------------------------------
  def alignment_Internal1(z_list: List[Short]) : Boolean = {
    (z_list(0) - z_list(1) - z_list(2) ) == 0
  }
  def alignment_ExternalInternal(z_list: List[Short]) : Boolean= {
    (z_list(0) + z_list(1) + z_list(2) - z_list(3)) == 0
  }
  def alignment_InternalExternal(z_list: List[Short]) : Boolean= {
    (z_list(0) - z_list(1) - z_list(2) - z_list(3)) == 0
  }
  def alignment_ExternalExternal(z_list: List[Short]) : Boolean= {
    (z_list(0) + z_list(1) - z_list(2) - z_list(3)) == 0
  }
  def alignment_InternalInternal(z_list: List[Short]) : Boolean= {
    (z_list(0) - z_list(1) + z_list(2) - z_list(3)) == 0
  }
  def alignment_External1(z_list: List[Short]) : Boolean= {
    (z_list(0) + z_list(1) - z_list(2) ) == 0
  }
  def alignment(z_list: List[Short], mechanismType : MechanismType) : Boolean = {
    mechanismType match {
      case Internal1 => alignment_Internal1(z_list)
      case ExternalExternal => alignment_ExternalExternal(z_list)
      case ExternalInternal => alignment_ExternalInternal(z_list)
      case InternalExternal => alignment_InternalExternal(z_list)
      case InternalInternal => alignment_InternalInternal(z_list)
      case External1 => alignment_External1(z_list)
    }
  }
}

import scala.collection.parallel._
object MechanismTypeDependentCalculations extends MechanismTypeDependentCalculations {

  //создает первичные массивы для трех(двух) первых колес
  def createArrays(z_min_max:  Array[(Short, Short)],mechanismType: MechanismType) : Array[ParVector[Short]] = {
    //место для хранения массивов
    val array : ArrayBuffer[ParVector[Short]] = ArrayBuffer.empty[ParVector[Short]]
    // --  этот комментарий не акутален ->//создаем коллекции и добавляем их (последнюю не трогаем пока, её мы вычислим на основе условия соосности
    //создаем все области
    for (i <- 0 until z_min_max.length- 1){
      array.addOne(ParVector.range(z_min_max(i)._1, z_min_max(i)._2))
    }
     // tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(5))
    array.toArray
  }
}