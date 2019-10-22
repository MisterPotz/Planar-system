package planar_structure.analysis.analyzers

import planar_structure.core_structure.links.{AbstractHolder, LinkElemHolder}
import planar_structure.core_structure.{Mechanism, MechanismImplicits}

import scala.language.implicitConversions
import scala.reflect.ClassTag

//FuncManager имеет два вложенных класса - юнайтер (объединитель) и продьюсер (тот кто производит элементарную функцию)
//продьюсер асбтрактен, таким образом можно наследоваться от него и заложить своего производителя элементарных функций
//юнайтер также абстрактен, в него можно заложить три типа функций - стартовая, промежуточная и финальная. стартовая -
//функция на которой наращивается большая конечная путем приращения мини-функций продьюсера.
//функции продьюсера объединяются промежуточной
//после сращения имеет возможность передать результат в финальную функцию-модификатор вывода
abstract class FuncManager[T, Input, Output]{
  //T - type of element to be passed
  type S = T
  type Inp = Input
  type Out = Output
  abstract class FuncProducer { //guy_who_analyzes_and_produces_elemental_func_with_given_args
    def produceFunc(pair : S) : (scala.collection.Seq[Input]) => Out
    def getFuncList(pairs_list : List[S])(implicit tag : ClassTag[T]) : List[(scala.collection.Seq[Input]) => Out] = {
      pairs_list.map{
        pair : S => produceFunc(pair)
      }
    }
  }
  abstract class FuncUniter{ //guy_who_connects_all_funcs
    //последнее действие, входной параметр - результат вычисления объединённой функции
    protected val arguments_amount_in_S_type : Int //количество аргументов на один элемент S
    protected val final_func : (Output) => Output
    def uniteFunc(func_list : List[(scala.collection.Seq[Input]) => Out]) : (scala.collection.Seq[Input]) => Out = {
      val calculated_Func = func_list.foldLeft[(scala.collection.Seq[Input])=>Out](
        (a : scala.collection.Seq[Input]) => start_func(a)) {
        (left, right) => {
          (seq : scala.collection.Seq[Input]) =>{
            middle_func(left(seq.dropRight(arguments_amount_in_S_type)), right(seq.drop(seq.length - arguments_amount_in_S_type)))
          }
        }
      }
      (a : collection.Seq[Input]) => final_func(calculated_Func(a))
    }
    //промежуточное действие, объединяющее функции
    protected val middle_func : (Output, Output) => Output
    //первая функция, на которой наращиваются следующие действия
    protected val start_func : (scala.collection.Seq[Input] => Output)
  }
  val producer : FuncProducer
  val uniter : FuncUniter
}

//скрейпер - имеет в себе объект функционального менеджера. является шапкой для методов менеджера
//T - type of elements in pairs, Input - type of argument for function, Output - type of result of resulting function
abstract class FunctionScraper[T, Input, Output]{
  val func_manager : FuncManager[T, Input, Output]
  //из пар зацеплений создать список функций
  def scrapeFuncs(pair_list : List[T])(implicit tag : ClassTag[T]) : List[(scala.collection.Seq[Input])=>Output] = {
    func_manager.producer.getFuncList(pair_list)
  }
  //объеденить список функций в одну
  def getResFunc(list : List[T])(implicit tag : ClassTag[T]) : (scala.collection.Seq[Input]) => Output = {
    func_manager.uniter.uniteFunc(scrapeFuncs(list))
  }
  def checkInputAndProduce(arguments_list : Input*) : Output
  def checkInputAndConfirm(arguments_list : Input*) : Boolean
}

trait Analyser
//аналайзер - тот кто использует скрейпера, знает какие аргументы передать скрейперу и как их подготовить
abstract class StructureAnalyser[T,Input, Output](val mechanism : Mechanism) extends MechanismImplicits with Analyser {
  val function_scraper : FunctionScraper[T,Input,Output]
  //выстроенные по порядку пару зацеплений
  val linearizedPairs : List[T]
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  val prepared_arg_holders_for_func : List[AbstractHolder]
  //значет как из prepared_arg_holders_for_func получить аргументы для найденной функции
  protected def extract_args : List[Input]
  //вычислить функцию и передать ей параметр
  def calcFunc : Output = calculatedFunc.apply(extract_args)
  //просто вычислить функцию
  def getFunc(implicit tag : ClassTag[T]) : (scala.collection.Seq[Input]) => Output = function_scraper.getResFunc(linearizedPairs)
  //хранит вычисленную функцию
  val calculatedFunc: collection.Seq[Input] => Output
  def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[Input, Output]]
}

abstract class MiniStructureAnalyzer[Input, Output](val saved_function : Function1[Input, Output],
                                                                                   val holder : LinkElemHolder) extends MechanismImplicits with Analyser{
  def prepare_arg : Input
  def calc : Output = saved_function(prepare_arg)
}
//теперь сделать так чтобы можно было непосредственно получить список функций во внешнее юзание, чтобы в цикле все было чуток понятнее и быстрее