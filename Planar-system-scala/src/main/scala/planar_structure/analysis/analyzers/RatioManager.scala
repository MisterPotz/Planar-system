package planar_structure.analysis

import planar_structure.core_structure.connections.{ExternalConnection, GearConnection, InternalConnection}
import planar_structure.core_structure.links.WheelHolder
import planar_structure.core_structure.{GearWheel, Mechanism}

import scala.collection.mutable.ListBuffer
//TODO сделать вариации передаточного соотношения
//TODO коэффициент торцового перекрытия
//TODO условие отсутствия интерференции
//TODO условие сборки
object FuncManagerGearRatio extends FuncManager[(GearWheel,GearWheel), Int, Double] {
  object FuncProducerGearRatio extends FuncProducer{
    override def produceFunc(pair: (GearWheel, GearWheel)): (scala.collection.Seq[Int]) => Double = {
      GearConnection.makeConnection(pair._1, pair._2) match {
        case Some(_: ExternalConnection) =>
          (seq: scala.collection.Seq[Int]) => {
          (-1.0) * seq(1).toDouble / seq(0).toDouble
        }
        case Some(_: InternalConnection) => (seq: scala.collection.Seq[Int]) => {
          seq(1).toDouble / seq(0).toDouble
        }
        case None => _ => 1.0
      }
    }
  }
  object FuncUniterGearRatio extends FuncUniter{
    override protected val middle_func: (Double, Double) => Double = _ * _
    override protected val start_func: collection.Seq[Int] => Double = _ => 1
    override protected val final_func: (Double) => Double = 1 - _
    override protected val arguments_amount_in_S_type: Int = 2
  }
  override val producer: FuncProducer= FuncProducerGearRatio
  override val uniter: FuncUniter = FuncUniterGearRatio
}

object FunctionScraperGearRatio extends FunctionScraper[(GearWheel,GearWheel), Int, Double]{
  override val func_manager: FuncManager[(GearWheel,GearWheel), Int, Double] = FuncManagerGearRatio
  override def checkInputAndProduce(arguments_list: Int*): Double = ???
  override def checkInputAndConfirm(arguments_list: Int*): Boolean = ???
}

class StructureAnalyzerRatio(override val mechanism: Mechanism) extends StructureAnalyser[(GearWheel,GearWheel),Int, Double](mechanism){
  override val function_scraper : FunctionScraper[(GearWheel,GearWheel), Int, Double] = FunctionScraperGearRatio
  //выстроенные по порядку пару зацеплений
  override val linearizedPairs: List[(GearWheel, GearWheel)] = mechanism.linearizePairsWith(GearConnection)
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  override val prepared_arg_holders_for_func: List[WheelHolder] = mechanism.linearizePairsWith(GearConnection).map(pair => (pair._1.holder, pair._2.holder))
    .foldLeft(ListBuffer.empty[WheelHolder])((left, right) => left.addOne(right._1).addOne(right._2)).toList
  override protected def extract_args : List[Int] = prepared_arg_holders_for_func.map(holder => holder.z)
  override lazy val calculatedFunc: collection.Seq[Int] => Double = getFunc

  override def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[Int, Double]] = ???
}
