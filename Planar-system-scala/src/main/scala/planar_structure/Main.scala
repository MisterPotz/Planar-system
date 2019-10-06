package planar_structure

import planar_structure.core_structure.connections.{ConnectionImplicits, GearConnection}
import planar_structure.core_structure.{Implicits, LinkElem, Mechanism, MechanismHolder, MechanismImplicits}

import scala.collection.mutable

object Main extends App with Implicits with ConnectionImplicits with MechanismImplicits{
  //TODO функция нахождения пригодных для соединений звеньев работает, но лучше потестить
  val mech = Mechanism(MechanismHolder.creator_funcs("One Row")())
  mech.installGearConnections()
  println(mech.toStringBrief)
  val mech2 = Mechanism(MechanismHolder.creator_funcs("Two Row Test")())
  mech2.installConnectionsWith(GearConnection)
  println(mech2.toStringBrief)
  val filtered_list = mech2.linearizePairsWith(GearConnection)
  println(filtered_list)
}