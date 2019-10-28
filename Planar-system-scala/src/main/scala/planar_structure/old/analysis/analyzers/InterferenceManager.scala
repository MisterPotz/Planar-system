package planar_structure.old.analysis.analyzers

import planar_structure.old.core_structure.connections.{ExternalConnection, GearConnection, InternalConnection}
import planar_structure.old.core_structure.links.WheelHolder
import planar_structure.old.core_structure.{GearWheel, Mechanism}
/*

import scala.collection.mutable.ListBuffer
//TODO коэффициент торцового перекрытия
//TODO условие отсутствия интерференции
object FuncManagerGearInterference extends FuncManager[(GearWheel,GearWheel), WheelHolder, Double] {
  object FuncProducerGearInterference extends FuncProducer{
    override def produceFunc(pair: (GearWheel, GearWheel)): (scala.collection.Seq[WheelHolder]) => Double = {
      GearConnection.makeConnection(pair._1, pair._2) match {
        case Some(_: ExternalConnection) =>
          (seq: scala.collection.Seq[WheelHolder]) => {
            (-1.0) * seq(1).toDouble / seq(0).toDouble
          }
        case Some(_: InternalConnection) => (seq: scala.collection.Seq[WheelHolder]) => {
          seq(1).toDouble / seq(0).toDouble
        }
        case None => _ => 1.0
      }
    }
  }
  object FuncUniterGearInterference extends FuncUniter{
    override protected val middle_func: (Double, Double) => Double = _ * _
    override protected val start_func: collection.Seq[WheelHolder] => Double = _ => 1
    override protected val final_func: (Double) => Double = 1 - _
    override protected val arguments_amount_in_S_type: Int = 2
  }
  override val producer: FuncProducer= FuncProducerGearInterference
  override val uniter: FuncUniter = FuncUniterGearInterference
}

object FunctionScraperGearInterference extends FunctionScraper[(GearWheel,GearWheel), WheelHolder, Double]{
  override val func_manager: FuncManager[(GearWheel,GearWheel), Int, Double] = FuncManagerGearInterference
  override def checkInputAndProduce(arguments_list: Int*): Double = ???
  override def checkInputAndConfirm(arguments_list: Int*): Boolean = ???
}

//by default calculates the ratio for 2K-H mechanisms with carrier as an output, so U^(b)_ah
class StructureAnalyzerInterference(override val mechanism: Mechanism) extends StructureAnalyser[(GearWheel,GearWheel),WheelHolder, Boolean](mechanism){
  override val function_scraper : FunctionScraper[(GearWheel,GearWheel), WheelHolder, Boolean] = FunctionScraperGearInterference
  //выстроенные по порядку пару зацеплений
  override val linearizedPairs: List[(GearWheel, GearWheel)] = mechanism.linearizePairsWith(GearConnection)
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  override val prepared_arg_holders_for_func: List[WheelHolder] = mechanism.linearizePairsWith(GearConnection).map(pair => (pair._1.holder, pair._2.holder))
    .foldLeft(ListBuffer.empty[WheelHolder])((left, right) => left.addOne(right._1).addOne(right._2)).toList
  override protected def extract_args : List[Int] = prepared_arg_holders_for_func.map(holder => holder.z)
  override lazy val calculatedFunc: collection.Seq[Int] => Double = getFunc

  override def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[Int, Double]] = ???
}
*/
