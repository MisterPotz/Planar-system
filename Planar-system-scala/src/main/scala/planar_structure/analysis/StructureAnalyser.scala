package planar_structure.analysis

import planar_structure.core_structure.connections.{ExternalConnection, GearConnection, InternalConnection}
import planar_structure.core_structure.links.WheelHolder
import planar_structure.core_structure.{GearWheel, Mechanism, MechanismImplicits}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

abstract class FuncManager[T, Input, Output]{
  type S = T
  type Inp = Input
  type Out = Output
  abstract class FuncProducer { //guy_who_analyzes_and_produces_elemental_func_with_given_args
    def produceFunc(pair : (S,S)) : (scala.collection.Seq[Input]) => Out
    def getFuncList(pairs_list : List[(S,S)]) : List[(scala.collection.Seq[Input]) => Out]
  }
  abstract class FuncUniter{ //guy_who_connects_all_funcs
    def uniteFunc(func_list : List[(scala.collection.Seq[Input]) => Out]) : (scala.collection.Seq[Input]) => Out
  }
  val producer : FuncProducer
  val uniter : FuncUniter
}

object FuncManagerGearRatio extends FuncManager[GearWheel, Int, Double] {
  object FuncProducerGearRatio extends FuncProducer{
    override def produceFunc(pair: (GearWheel, GearWheel)): (scala.collection.Seq[Int]) => Double = {
      GearConnection.makeConnection(pair._1, pair._2) match {
        case Some(_ : ExternalConnection) => (seq : scala.collection.Seq[Int]) => {(-1.0) * seq(1).toDouble / seq(0).toDouble}
        case Some(_: InternalConnection) => (seq : scala.collection.Seq[Int]) => {seq(1).toDouble/ seq(0).toDouble}
        case None => _ => 1.0
      }
    }
    override def getFuncList(pairs_list : List[(GearWheel, GearWheel)]) : List[(scala.collection.Seq[Int]) => Double] = {
      pairs_list.map{
        pair : (GearWheel, GearWheel) =>
          produceFunc(pair)
      }
    }
  }
  object FuncUniterGearRatio extends FuncUniter{
    override def uniteFunc(func_list: List[collection.Seq[Int] => Double]): (scala.collection.Seq[Int]) => Double = {
      val paired_indeces : List[(Int, Int)] = {for (i <- 0 until func_list.length * 2) yield i}
        .grouped(2).map(pair => (pair(0), pair(1))).toList
      val zipped_func_list = func_list.zip(paired_indeces)
      val calculated_Func = zipped_func_list.foldLeft[((scala.collection.Seq[Int])=>Double,ListBuffer[Int])](
        ((_ : scala.collection.Seq[Int]) => 1.0, ListBuffer.empty[Int])) {
        (left, right) => {
          ((seq : scala.collection.Seq[Int]) => left._1(seq) * right._1(scala.collection.Seq(seq(right._2._1), seq(right._2._2))), //left._2.toSeq
          ({left._2.addAll(scala.collection.Seq[Int](right._2._1, right._2._2)); left._2}))
        }
      }._1
      (a : collection.Seq[Int]) => 1 - calculated_Func(a)
    }
  }
  override val producer: FuncProducer= FuncProducerGearRatio
  override val uniter: FuncUniter = FuncUniterGearRatio
}

//T - type of elements in pairs, Input - type of argument for function, Output - type of result of resulting function
abstract class FunctionScraper[T, Input, Output]{
  val func_manager : FuncManager[T, Input, Output]
  def scrapeFuncs(pair_list : List[(T,T)]) : List[(scala.collection.Seq[Input])=>Output] = {
    val buffer = ListBuffer.empty[(scala.collection.Seq[Input])=>Output]
    pair_list.foldLeft(buffer)((left, right) => left.addOne(func_manager.producer.produceFunc(right))).toList
  }
  def getResFunc(list : List[(T,T)]) : (scala.collection.Seq[Input]) => Output = {
    func_manager.uniter.uniteFunc(scrapeFuncs(list))
  }
  //TODO надо сделать так, чтобы взять необходимые ссылки на целевые переменные, получить функцию и передавать туда эти переменные при запросе
  def checkInputAndProduce(arguments_list : Input*) : Output
  def checkInputAndConfirm(arguments_list : Input*) : Boolean
}


object FunctionScraperGearRatio extends FunctionScraper[GearWheel, Int, Double]{
  override val func_manager: FuncManager[GearWheel, Int, Double] = FuncManagerGearRatio
  override def checkInputAndProduce(arguments_list: Int*): Double = ???
  override def checkInputAndConfirm(arguments_list: Int*): Boolean = ???
}

//задача - написать функцию, вычисляющую функцию для нахождения передаточногро соотношения
class StructureAnalyser(val mechanism : Mechanism) extends MechanismImplicits{
  //знает, какой массив соединяемыъ пар отправили, и знает какие аргументы кидать в результирующую функцию чтобы вычислить
  abstract class CalcFuncAndApply[Input, Output]{
    def calcFunc : Output
    def getFunc : (scala.collection.Seq[Input]) => Output
  }
  class CalcFuncAndApplyRatio extends CalcFuncAndApply[Int, Double]{
    val linearizedPairs: List[(GearWheel, GearWheel)] = mechanism.linearizePairsWith(GearConnection)
    val prepared_arg_holders_for_func: List[WheelHolder] = mechanism.linearizePairsWith(GearConnection).map(pair => (pair._1.holder, pair._2.holder))
      .foldLeft(ListBuffer.empty[WheelHolder])((left, right) => left.addOne(right._1).addOne(right._2)).toList
    def extract_args : List[Int] = prepared_arg_holders_for_func.map(holder => holder.z)
    lazy val calculatedFunc: collection.Seq[Int] => Double = getFunc
    override def calcFunc: Double = calculatedFunc.apply(extract_args)
    override def getFunc: collection.Seq[Int] => Double = FunctionScraperGearRatio.getResFunc(linearizedPairs)
  }
  lazy val ratio_calculator : CalcFuncAndApplyRatio = new CalcFuncAndApplyRatio
}
