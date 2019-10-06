package planar_structure.analysis

import planar_structure.core_structure.MechanismImplicits

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

abstract class FuncManager[T, Input, Output]{
  type S = T
  type Inp = Input
  type Out = Output
  abstract class FuncProducer[S] { //guy_who_analyzes_and_produces_elemental_func_with_given_args
    def produceFunc(pair : (S,S)) : (Inp*) => Out
  }
  abstract class FuncUniter[S] { //guy_who_connects_all_funcs

  }
  var producer : FuncProducer[S]
  var uniter : FuncUniter[S]
}

//T - type of elements in pairs, Input - type of argument for function, Output - type of result of resulting function
abstract class FunctionScraper[T, Input, Output]{
  val func_manager : FuncManager[T, Input, Output]
  def scrapeFuncs(pair_list : List[(T,T)]) : List[(Input*)=>Output] = {
    val buffer = ListBuffer.empty[(Input*)=>Output]
    pair_list.foldLeft(buffer)((left, right) => left.addOne(func_manager.producer.produceFunc(right))).toList
  }
  def getResFunc(func_list : List[(Input*) => Output]) : (Input*) => Output
  def checkInputAndProduce(arguments_list : Input*) : Output
  def checkInputAndConfirm(arguments_list : Input*) : Boolean
}

//задача - написать функцию, вычисляющую функцию для нахождения передаточногро соотношения
object StructureAnalyser extends MechanismImplicits{
  def getRatioFunc(mecha)
}
