package planar_structure

import planar_structure.core_structure.connections.ConnectionImplicits
import planar_structure.core_structure.{Implicits, LinkElem, Mechanism, MechanismHolder, MechanismImplicits}

import scala.collection.mutable

object Main extends App with Implicits with ConnectionImplicits with MechanismImplicits{
  //TODO функция нахождения пригодных для соединений звеньев работает, но лучше потестить
  val mech = Mechanism(MechanismHolder.creator_funcs("One Row")())
  mech.installConnections()
  println(mech.toStringBrief)
  val mech2 = Mechanism(MechanismHolder.creator_funcs("Two Row EI")())
  mech2.installConnections()
  println(mech2.toStringBrief)
  }