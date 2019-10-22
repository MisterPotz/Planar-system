package planar_structure.analysis

import planar_structure.core_structure.connections.GearConnection
import planar_structure.core_structure.links.SatelliteHolder
import planar_structure.core_structure.{GearWheel, Mechanism, Satellite}

object FuncManagerNeighborhoodCondition extends FuncManager[Satellite, (Double,Double), Boolean]{
  object FuncProducerNeighborhoodCondition extends FuncProducer{
    override def produceFunc(pair: Satellite): (scala.collection.Seq[(Double,Double)]) => Boolean = {
      (seq: scala.collection.Seq[(Double, Double)]) => seq(0)._1 < 2 * seq(0)._2 * math.sin(math.Pi / pair.holder.satellitesAmount)
    }
  }
  object FuncUniterNeighborhoodCondition extends FuncUniter{
    override protected val middle_func: (Boolean, Boolean) => Boolean = _  & _
    override protected val start_func: collection.Seq[(Double, Double)] => Boolean = _ => true
    override protected val final_func: (Boolean) => Boolean =  (a) => a
    override protected val arguments_amount_in_S_type: Int = 1
  }
  override val producer: FuncProducer= FuncProducerNeighborhoodCondition
  override val uniter: FuncUniter = FuncUniterNeighborhoodCondition
}

object FunctionScraperNeighborhoodCondition extends FunctionScraper[Satellite, (Double,Double), Boolean]{
  override val func_manager: FuncManager[Satellite, (Double,Double), Boolean] = FuncManagerNeighborhoodCondition
  override def checkInputAndProduce(arguments_list: (Double,Double)*): Boolean = ???
  override def checkInputAndConfirm(arguments_list: (Double,Double)*): Boolean = ???
}

class StructureAnalyzerNeighborhoodCondition(override val mechanism: Mechanism) extends StructureAnalyser[Satellite, (Double, Double), Boolean](mechanism){
  override val function_scraper : FunctionScraper[Satellite, (Double, Double), Boolean] = FunctionScraperNeighborhoodCondition
  //выстроенные по порядку пару зацеплений
  override val linearizedPairs: List[Satellite] = mechanism.getLinksRawOfType[Satellite]
  //полученные держатели значений элемента (холдеры), инкапсулирующие параметры звена (например, число зубьев)
  override val prepared_arg_holders_for_func: List[SatelliteHolder] = new SatelliteHolder() :: Nil
  def maxWheel : GearWheel = {
    val found = prep_wheels_list.maxBy(wheel => mechanism.findConnectionWith(wheel).asInstanceOf[GearConnection].holder.get_da_by(wheel.holder))
   // println(s"Found max wheel: ${found}; z: ${found.holder.z}")
    found
  }
  val prep_wheels_list : List[GearWheel] = mechanism.mech_holder.linkSeq.elems.find(b=> b match {case a : Satellite => true; case _ => false}).get.asInstanceOf[Satellite].crown_wheels //mechanism.getLinkOfType[Satellite](0).get.crown_wheels
  var prep_connection : GearConnection = mechanism.findConnectionWith(maxWheel).asInstanceOf[GearConnection] //TODO здесь не идет обновление
  override protected def extract_args : List[(Double, Double)] = {
    val max_wheel = maxWheel
    List((prep_connection.holder.get_da_by(max_wheel.holder), prep_connection.holder.aw))
  }
  override lazy val calculatedFunc: collection.Seq[(Double, Double)] => Boolean = getFunc
  def getMinimizedStructureAnalyzers: List[MiniStructureAnalyzer[(Double,Double), Boolean]] = ???
}
