package planar_structure.old.analysis.analyzers

import planar_structure.old.core_structure.connections.GearConnection
import planar_structure.old.core_structure.links.SatelliteHolder
import planar_structure.old.core_structure.{GearWheel, Mechanism, Satellite}

//все необходимое берем из сателлита (число зубьев первого колеса)
//передаем только передаточное соотношение и число сателлитов
object FuncManagerAssemblyCondition extends FuncManager[Satellite, (Double, Double), Boolean]{
  object FuncProducerAssemblyCondition extends FuncProducer{
    override def produceFunc(pair: Satellite): (scala.collection.Seq[(Double,Double)]) => Boolean = {
      val crown_holder = pair.crown_wheels(0).holder
      (seq: scala.collection.Seq[(Double, Double)]) => {
          val u1h = seq(0)._1
          val k = seq(0)._2
          var flag : Boolean = true
          for (i <- Range(1,7)){
            // TODO здесь возможна оптимизация через брейк
            //TODO сравнить то что эта штука выводит с нормальными результатами (по кудрявцеву пример не прошел, по методе прошел

            val res = u1h * crown_holder.z / k * (1 + i * k)
            //проверить это условие сборки
            if (math.abs(res - res.round) > 0.001){
              val res_ = res.round
              flag = false
            }
          }
          flag
      }
    }
  }
  object FuncUniterAssemblyCondition extends FuncUniter{
    override protected val middle_func: (Boolean, Boolean) => Boolean = _  & _
    override protected val start_func: collection.Seq[(Double, Double)] => Boolean = _ => true
    override protected val final_func: (Boolean) => Boolean =  (a) => a
    override protected val arguments_amount_in_S_type: Int = 1
  }
  override val producer: FuncProducer= FuncProducerAssemblyCondition
  override val uniter: FuncUniter = FuncUniterAssemblyCondition
}

object FunctionScraperAssemblyCondition extends FunctionScraper[Satellite, (Double,Double), Boolean]{
  override val func_manager: FuncManager[Satellite, (Double,Double), Boolean] = FuncManagerAssemblyCondition
  override def checkInputAndProduce(arguments_list: (Double,Double)*): Boolean = ???
  override def checkInputAndConfirm(arguments_list: (Double,Double)*): Boolean = ???
}

class StructureAnalyzerAssemblyCondition(override val mechanism: Mechanism) extends StructureAnalyser[Satellite, (Double, Double), Boolean](mechanism){
  override val function_scraper : FunctionScraper[Satellite, (Double, Double), Boolean] = FunctionScraperAssemblyCondition
  //что мы будем передавать в построение дополнительно
  override val linearizedPairs: List[Satellite] = mechanism.getLinksRawOfType[Satellite]
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  override val prepared_arg_holders_for_func: List[SatelliteHolder] = linearizedPairs(0).holder :: Nil
  val ratio : StructureAnalyzerRatio = new StructureAnalyzerRatio(mechanism)
  override protected def extract_args : List[(Double, Double)] = {
    List((ratio.calcFunc, mechanism.mech_holder.satellite_amount))
  }
  override lazy val calculatedFunc: collection.Seq[(Double, Double)] => Boolean = getFunc
  def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[(Double,Double), Boolean]] = ???
}
