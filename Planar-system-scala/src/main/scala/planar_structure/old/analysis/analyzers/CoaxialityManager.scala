package planar_structure.old.analysis.analyzers

import planar_structure.old.core_structure.connections.{GearConnection, GearConnectionHolder}
import planar_structure.old.core_structure.{GearWheel, Mechanism}

object FuncManagerCoaxiality extends FuncManager[(GearWheel, GearWheel), Double, Double]{
  object FuncProducerCoaxiality extends FuncProducer{
    override def produceFunc(pair: (GearWheel, GearWheel)): (scala.collection.Seq[Double]) => Double = {
      if (pair._2.position_holder.second_axis_higher_than_first) {
        (seq: scala.collection.Seq[Double]) => seq(0)
      } else {
        (seq: scala.collection.Seq[Double]) => -seq(0)
      }
      //val z_min = (2 * (pair.holder.hta - pair.holder.xt) / math.pow(math.sin(pair.holder.alpha_t), 2)).ceil
      //println(s"Min z is ${z_min}")
      //(seq : scala.collection.Seq[Double]) => 1.0
    }
  }
  object FuncUniterCoaxiality extends FuncUniter{
    override protected val middle_func: (Double, Double) => Double = _  +  _
    override protected val start_func: collection.Seq[Double] => Double = _ => 0.0
    override protected val final_func: (Double) => Double =  (a) => a
    override protected val arguments_amount_in_S_type: Int = 1
  }
  override val producer: FuncProducer= FuncProducerCoaxiality
  override val uniter: FuncUniter = FuncUniterCoaxiality
}

object FunctionScraperCoaxiality extends FunctionScraper[(GearWheel, GearWheel), Double, Double]{
  override val func_manager: FuncManager[(GearWheel, GearWheel), Double, Double] = FuncManagerCoaxiality
  override def checkInputAndProduce(arguments_list: Double*): Double = ???
  override def checkInputAndConfirm(arguments_list: Double*): Boolean = ???
}

class StructureAnalyzerCoaxiality(override val mechanism: Mechanism) extends StructureAnalyser[(GearWheel, GearWheel),Double, Double](mechanism){
  override val function_scraper : FunctionScraper[(GearWheel,GearWheel), Double, Double] = FunctionScraperCoaxiality
  //выстроенные по порядку пару зацеплений
  override val linearizedPairs: List[(GearWheel, GearWheel)] = mechanism.linearizePairsWith(GearConnection)
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  override val prepared_arg_holders_for_func: List[GearConnectionHolder] = linearizedPairs.map(mechanism.mech_holder.controlMap.holder(_)).map(connection => connection.holder.asInstanceOf[GearConnectionHolder])
  override protected def extract_args : List[Double] = prepared_arg_holders_for_func.map(holder => holder.aw)
  override lazy val calculatedFunc: collection.Seq[Double] => Double = getFunc
  def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[Double, Double]] = ???
}
