package planar_structure

import planar_structure.analysis.StructureAnalyser
import planar_structure.core_structure.connections.{ConnectionImplicits, GearConnection}
import planar_structure.core_structure.{GearWheel, Implicits, LinkElem, Mechanism, MechanismHolder, MechanismImplicits}

import scala.collection.mutable

object Main extends App with Implicits with ConnectionImplicits with MechanismImplicits{
  //TODO функция нахождения пригодных для соединений звеньев работает, но лучше потестить
  /*val mech = Mechanism(MechanismHolder.creator_funcs("One Row")())
  mech.installGearConnections()
  println(mech.toStringBrief)
  val analyzer : StructureAnalyser = new StructureAnalyser(mech)
  mech.getLinkOfType[GearWheel](0).get.holder.z = 40
  mech.getLinkOfType[GearWheel](1).get.holder.z = 30
  mech.getLinkOfType[GearWheel](2).get.holder.z = 160
  mech.updateConnections()
  println(analyzer.ratio_calculator.calcFunc)*/
  val mech2rowee = Mechanism(MechanismHolder.creator_funcs("Two Row EE")())
  val elems_of_mech2rowee = mech2rowee.getLinksOfType[GearWheel]
  elems_of_mech2rowee(0).holder.z = 40
  elems_of_mech2rowee(1).holder.z = 30
  elems_of_mech2rowee(2).holder.z = 48
  elems_of_mech2rowee(3).holder.z = 24
  mech2rowee.installGearConnections()
  val analyzer_mech2rowee = new StructureAnalyser(mech2rowee)
  println(mech2rowee.toStringBrief)
  println(s"Numbers of teeths for each wheel: ${analyzer_mech2rowee.ratio_calculator.extract_args}")
  println(s"Calculated gear ratio: ${analyzer_mech2rowee.ratio_calculator.calcFunc}")
}