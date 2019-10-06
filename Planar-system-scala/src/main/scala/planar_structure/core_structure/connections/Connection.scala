package planar_structure.core_structure.connections

import planar_structure.core_structure.links.WheelHolder
import planar_structure.core_structure.{ExternalWheel, GearWheel, Implicits, InternalWheel, LinkElem, LinkSeq}
import planar_structure.help_traits.BeautifulDebugOutput

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//соединение.
sealed abstract class Connection(holder : ConnectionHolder) extends BeautifulDebugOutput{
  override def toStringFull: String = holder.toStringFull
  override def toString: String = holder.toString
  override def toStringShort: String = holder.toStringShort
}
case class ConnectionMap(holder : mutable.HashMap[(LinkElem, LinkElem), Connection])
object ConnectionMap{
  def empty :ConnectionMap = ConnectionMap(mutable.HashMap.empty[(LinkElem, LinkElem), Connection])
}

sealed abstract class GearConnection(holder : GearConnectionHolder) extends Connection(holder)
case class InternalConnection(holder : InternalConnectionHolder) extends GearConnection(holder)
case class ExternalConnection(holder : ExternalConnectionHolder) extends GearConnection(holder)


object GearConnection{
  //creates the nessecary connections from input GearWheels which incapsulATE wheel holders
  def apply(first_ : GearWheel, second_ : GearWheel) : GearConnection = {
    (first_, second_) match {
      case (ExternalWheel(holder1), ExternalWheel(holder2)) => ExternalConnection(new ExternalConnectionHolder(holder1, holder2))
      case (InternalWheel(holder1),ExternalWheel(holder2)) => InternalConnection(new InternalConnectionHolder(holder1, holder2))
      case (ExternalWheel(holder1), InternalWheel(holder2)) => InternalConnection(new InternalConnectionHolder(holder1, holder2))
      case _ => throw new IllegalArgumentException("Can't create connection with given wheels")
    }
  }
  def areConnectable(checked : (GearWheel, GearWheel)) : Boolean = {
    checked match {
      case (ExternalWheel(_), ExternalWheel(_)) => true
      case (InternalWheel(_),ExternalWheel(_)) => true
      case  (ExternalWheel(_), InternalWheel(_)) => true
      case _ => false
    }
  }
}
trait ConnectionImplicits extends Implicits{
  implicit class ConnectionListOps(list : List[Connection]) extends  BeautifulDebugOutput {
    override def toString: String = toStringShort
    override def toStringShort: String = (list.foldLeft("")(_ + "\n" + _.toStringShort).drop(1))
    override def toStringFull: String = (list.foldLeft("")(_ + "\n" + _.toStringFull).drop(1))
  }
  implicit class ConnectionMapOps(map : mutable.HashMap[(LinkElem, LinkElem), Connection]){
    def installConnections(seq : LinkSeq) : mutable.HashMap[(LinkElem, LinkElem), Connection] = {
      seq.prepareAllPairs.foreach {
        //create new connection if we see gearwheels in a pair
        case ( first : GearWheel, second : GearWheel) => map.addOne( (first, second) -> GearConnection.apply(first, second))
        case _ => map
      }
      map
    }
    //linearize connections in right order with givern initial LinkSeq object
    def linearize(seq : LinkSeq) : List[Connection] = {
      val buff : ListBuffer[Connection] = ListBuffer.empty[Connection]
      val list = map.toList
      seq.prepareAllPairs.foreach( pair => list.find((key_pair) => (key_pair._1 == pair)) match {
        case Some(key_pair) => buff.addOne(key_pair._2)
        case None => buff
      })
      buff.toList
    }
    def getConnection(i : Int)(implicit seq : LinkSeq) : Connection = {
      linearize(seq)(i)
    }
  }
  implicit class ConnectionOps(conn : ConnectionMap) extends ConnectionMapOps(conn.holder)
}