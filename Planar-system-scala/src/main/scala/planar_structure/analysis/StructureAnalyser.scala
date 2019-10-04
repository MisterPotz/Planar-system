package planar_structure.analysis

import scala.language.implicitConversions
import planar_structure.core_structure.{BaseGearWheel, BaseLink, GearConnection, RecognizableBaseLink}
import planar_structure.mechanisms.Mechanism

import scala.collection.mutable.ListBuffer

//contains functions which are used to create functions for fast run-time analysis of the mechanism structure
object StructureAnalyser extends  RecognizableBaseLink {
  def getRatioFunc(mechanism: Mechanism) : List[BaseLink] = {
    //TODO doesn't work, satellite cannot be cast to basewheel, even with implicit conversions. something bad happens
   val list : List[BaseLink] = mechanism.input_layer.getAllLinksAllowedForConnectionFull.toList.flatMap(
     a => {
       val left = a._1.asInstanceOf[BaseGearWheel]
       val right = a._2.asInstanceOf[BaseGearWheel]
       mechanism.input_layer.connections_storage.collection((left, right)).asInstanceOf[BaseLink]})
    list
  }

}

