
package planar_structure.analysis.analyzers

import planar_structure.core_structure.links.WheelHolder
import planar_structure.core_structure.{GearWheel, GearWheelChecker, Mechanism}

object FuncManagerTeethAmount extends FuncManager[GearWheel, Int, Boolean] {
  object FuncProducerTeethAmount extends FuncProducer{
    override def produceFunc(pair: GearWheel): (scala.collection.Seq[Int]) => Boolean = {
      //делаем вот че: для каждого коеса свое условие, так как модули могут быть разные
      val z_min = (2 * (pair.holder.hta - pair.holder.xt) / math.pow(math.sin(pair.holder.alpha_t), 2)).ceil
          //println(s"Min z is ${z_min}")
      (seq : scala.collection.Seq[Int]) => seq(0) >=  z_min
    }
  }
  object FuncUniterTeethAmount extends FuncUniter{
    override protected val middle_func: (Boolean, Boolean) => Boolean = _  &  _
    override protected val start_func: collection.Seq[Int] => Boolean = _ => true
    override protected val final_func: (Boolean) => Boolean =  (a) => a
    override protected val arguments_amount_in_S_type: Int = 1
  }
  override val producer: FuncProducer= FuncProducerTeethAmount
  override val uniter: FuncUniter = FuncUniterTeethAmount
}

object FunctionScraperTeethAmount extends FunctionScraper[GearWheel, Int, Boolean]{
  override val func_manager: FuncManager[GearWheel, Int, Boolean] = FuncManagerTeethAmount
  override def checkInputAndProduce(arguments_list: Int*): Boolean = ???
  override def checkInputAndConfirm(arguments_list: Int*): Boolean = ???
}

class StructureAnalyzerTeethAmount(override val mechanism: Mechanism) extends StructureAnalyser[GearWheel,Int, Boolean](mechanism){
  override val function_scraper : FunctionScraper[GearWheel, Int, Boolean] = FunctionScraperTeethAmount
  //выстроенные по порядку пару зацеплений
  override val linearizedPairs: List[GearWheel] = mechanism.linearizeElemsWith(GearWheelChecker)
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  override val prepared_arg_holders_for_func: List[WheelHolder] = linearizedPairs.map(_.holder)
  override protected def extract_args : List[Int] = prepared_arg_holders_for_func.map(holder => holder.z)
  override lazy val calculatedFunc: collection.Seq[Int] => Boolean = getFunc

  def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[Int, Boolean]] = {
    function_scraper.func_manager.producer.getFuncList(linearizedPairs).zipWithIndex.map((func) =>
      new MiniStructureAnalyzer[Int, Boolean]((a) => func._1(Seq(a)),  holder =  prepared_arg_holders_for_func(func._2)) {
        override def prepare_arg: Int = holder.asInstanceOf[WheelHolder].z
      }
    )
  }

}
