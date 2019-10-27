package planar_structure

import planar_structure.analysis.analyzers.{StructureAnalyzerAssemblyCondition, StructureAnalyzerCoaxiality, StructureAnalyzerNeighborhoodCondition, StructureAnalyzerRatio, StructureAnalyzerTeethAmount}
import planar_structure.core_structure.connections.{ConnectionImplicits, GearConnection}
import planar_structure.core_structure.{GearWheel, Implicits, Mechanism, MechanismHolder, MechanismImplicits}

import scala.collection.mutable

object Main extends App with Implicits with ConnectionImplicits with MechanismImplicits{
  /*val mech = Mechanism(MechanismHolder.creator_funcs("One Row")())
  mech.installGearConnections()
  println(mech.toStringBrief)
  val analyzer : StructureAnalyser = new StructureAnalyser(mech)
  mech.getLinkOfType[GearWheel](0).get.holder.z = 40
  mech.getLinkOfType[GearWheel](1).get.holder.z = 30
  mech.getLinkOfType[GearWheel](2).get.holder.z = 160
  mech.updateConnections()
  println(analyzer.ratio_calculator.calcFunc)*/
  val mech2rowee = new Mechanism(MechanismHolder.creator_funcs("Two Row EI")())
  val elems_of_mech2rowee = mech2rowee.getLinksOfType[GearWheel]
  elems_of_mech2rowee(0).holder.z = 24
  elems_of_mech2rowee(1).holder.z = 48 //27
  elems_of_mech2rowee(2).holder.z = 48
  elems_of_mech2rowee(3).holder.z = 120
  mech2rowee.installGearConnections()
  val analyzer_mech2rowee = new StructureAnalyzerRatio(mech2rowee)
  println(mech2rowee.toStringBrief)
  println(s"Calculated gear ratio: ${analyzer_mech2rowee.calcFunc}")
  val teeth = new StructureAnalyzerTeethAmount(mech2rowee)
  println(s"There is no pruning in mechanism: ${teeth.calcFunc}")
  val coaxiality = new StructureAnalyzerCoaxiality(mech2rowee)
  println(s"Calculated coaxiality shift: ${coaxiality.calcFunc}")
  val neighborhood = new StructureAnalyzerNeighborhoodCondition(mech2rowee)
  println(s"Neighborhood condition satisfied: ${neighborhood.calcFunc}")
  val assemblyCondition = new StructureAnalyzerAssemblyCondition(mech2rowee)
  println(s"Assembly condition satisfied: ${assemblyCondition.calcFunc}")




 /* val mechanism = Mechanism(MechanismHolder.creator_funcs("Two Row EE")())
  val elems_of_mech2rowee1 = mechanism.getLinksOfType[GearWheel]
  elems_of_mech2rowee1(0).holder.z = 26
  elems_of_mech2rowee1(1).holder.z = 73 //27
  elems_of_mech2rowee1(2).holder.z = 18
  elems_of_mech2rowee1(3).holder.z = 81
  elems_of_mech2rowee1(0).holder.beta = math.toRadians(8.0)
  elems_of_mech2rowee1(1).holder.beta = math.toRadians(8.0) //27
  elems_of_mech2rowee1(2).holder.beta = math.toRadians(8.0)
  elems_of_mech2rowee1(3).holder.beta = math.toRadians(8.0)
  elems_of_mech2rowee1(0).holder.m = 2
  elems_of_mech2rowee1(1).holder.m = 2 //27
  elems_of_mech2rowee1(2).holder.m = 3
  elems_of_mech2rowee1(3).holder.m = 3
  mechanism.installGearConnections()
  println(s"Mechanism: ${mechanism.toStringBrief}")
  println(s"Mechanism mt first connection: ${mechanism.mech_holder.linkSeq.elems(1).asInstanceOf[GearWheel].holder.m / math.cos(mechanism.mech_holder.linkSeq.elems(1).asInstanceOf[GearWheel].holder.beta)}")
  println(s"Mechanism mt first connection: ${elems_of_mech2rowee1(2).holder.m / math.cos(elems_of_mech2rowee1(2).holder.beta)}")
  mechanism.linearizeConnections.foreach((connection) => println(s"Connection aw: ${connection.asInstanceOf[GearConnection].holder.aw}"))

  val analyzer_mech2rowee1 = new StructureAnalyzerRatio(mechanism)
  println(mechanism.toStringBrief)
  println(s"Calculated gear ratio: ${ 1 + math.abs(analyzer_mech2rowee1.calcFunc)}")
  val teeth1 = new StructureAnalyzerTeethAmount(mechanism)
  println(s"There is no pruning in mechanism: ${teeth1.calcFunc}")
  val coaxiality1 = new StructureAnalyzerCoaxiality(mechanism)
  println(s"Calculated coaxiality shift: ${coaxiality1.calcFunc}")*/
}
