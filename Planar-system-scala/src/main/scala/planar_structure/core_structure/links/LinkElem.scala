package planar_structure.core_structure

import planar_structure.core_structure.connections.GearConnection
import planar_structure.core_structure.links.{ExternalWheelHolder, InternalWheelHolder, SatelliteHolder, WheelHolder}
import planar_structure.help_traits.BeautifulDebugOutput

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.collection.immutable.List
import scala.collection.mutable

//base link class and its case subs
sealed abstract class LinkElem extends BeautifulDebugOutput
case class Carrier() extends LinkElem {
  override def toStringShort: String = "Carrier"
  override def toString: String = toStringShort
}

sealed abstract class LinkControlElem extends LinkElem
case class Input() extends  LinkControlElem {
  override def toStringShort: String = "Input"
  override def toString: String = toStringShort
}
case class Output() extends  LinkControlElem{
  override def toStringShort: String = "Output"
  override def toString: String = toStringShort
}

case class LinkSeq(elems: List[LinkElem]) extends LinkElem with Implicits {
  override def toStringShort: String = elems.toStringShort
  override def toString: String = toStringShort
}
case class Satellite(holder : SatelliteHolder) extends  LinkElem with Implicits{
  override def toStringShort: String = s"Satellite, ${holder.crowns.knownSize} crown(s): " +  BeautifulDebugOutput.print(holder.crowns.toStringShort)
  override def toString: String = toStringShort
  def crowns: mutable.HashMap[Int, LinkSeq] = holder.crowns
}
object GearWheel{
  def unapply(arg: GearWheel): Option[WheelHolder] = Option(arg) map {arg => arg.holder}
}

sealed abstract  class GearWheel(val holder : WheelHolder) extends  LinkElem {
  override def toStringShort: String = holder.toStringShort
  override def toString: String = toStringShort
}
case class InternalWheel(override val holder : InternalWheelHolder) extends GearWheel(holder){
  override def toStringShort: String = holder.toStringShort
  override def toString: String = toStringShort
}
case class ExternalWheel(override val holder : ExternalWheelHolder) extends GearWheel(holder){
  override def toStringShort: String = holder.toStringShort
  override def toString: String = toStringShort
}

trait Linearizable {
  def linearize : List[LinkElem]
}

trait LinkSeqOpsT extends  BeautifulDebugOutput{
  def getLink(i : Int) : LinkElem
//  //appending unique element to storages
//  def add(a: LinkElem): LinkSeqOpsT
//  def addAll(all: LinkElem*): LinkSeqOpsT
  //create or update connections
  //full re-init of current connections (in case wheels changes and we want to re-evaluate inner parameters of connections)
  def getBranchSize : Int
  def copy : LinkSeq
}

trait Implicits {
  implicit class SatelliteMapOps(sat : mutable.HashMap[Int, LinkSeq]) extends Linearizable with BeautifulDebugOutput {self=>
    override def linearize : List[LinkElem] = {
      //getting list like this one: List(0, -2, -1, 1, 2)
      //getting the elements in right position
      sat(0).elems ++ sat.toList.filter(_._1 != 0).sortWith(_._1 < _._1).foldLeft(List.empty[LinkElem])(
        (left : List[LinkElem], right : (Int, LinkSeq)) =>
        left concat right._2.elems)
    }
    def linearizeLists : List[LinkSeq] = {
      sat.toList.filter(_._1!= 0).sortWith(_._1 < _._1).foldLeft(List(sat(0)))((left, right) => left appended(right._2))
    }
    override def toStringShort: String = {
      val liniarized = sat.linearizeLists
      val sub = BeautifulDebugOutput.subFirst _
      liniarized.foldLeft("")((left, right) => left + "\n" + sub(right.toStringShort))
    }
    def getInputElem : LinkElem  = sat(0).elems(0)
  }
  implicit class SatelliteOps(sat : Satellite) extends SatelliteMapOps(sat.holder.crowns)
  //----------------------------------------
  implicit class LinkSeqListOps(list : List[LinkElem]) extends LinkSeqOpsT {self =>
    override def getLink(i: Int): LinkElem = {
      @scala.annotation.tailrec //annotation for compiler tail recursion optimization
      def getLinkRec(xs : List[LinkElem], lookingFor : Int):LinkElem = {
        if (lookingFor == 0)
          xs.head
        else {
          val tail = xs.head match {
              case a: Satellite => a.crowns.linearize ++ xs.tail
              case _ => xs.tail
          }
          getLinkRec(tail, lookingFor-1)
        }
      }
      getLinkRec(list, i)
    }
    /*override def add(a: LinkElem): LinkSeqOpsT = ???
    override def addAll(all: LinkElem*): LinkSeqOpsT = ???*/
    override def getBranchSize: Int = {
      list.foldLeft(0)((left, right) =>
        left + (right match {
          case sat : Satellite => sat.crowns.toList.foldLeft(0)((left, right) => left + right._2.getBranchSize)
          case _ => 1
        }))
    }
    override def copy: LinkSeq = ???
    override def toStringShort: String = list.foldLeft("")((left : String, right : LinkElem) => left + "\n" + right.toStringShort).drop(1)
    //used for making an arr with prepared pairs of connectable wheels, which can be used for TMM analysis
    def prepareAllPairs : List[(LinkElem, LinkElem)] = {
      @scala.annotation.tailrec
      def prepareAllPairsRec(list : List[LinkElem], list_to_append_to : ListBuffer[(LinkElem, LinkElem)]) : List[(LinkElem, LinkElem)] = {
        if ((list == Nil) || list.isEmpty || list.tail.isEmpty) {
          list_to_append_to.toList
        }
        else{
          val right = list.tail.head
          right match {
            case sat : Satellite =>
              val lists = sat.linearizeLists
              list_to_append_to.addOne((list.head, sat.getInputElem))
              lists.foldLeft(list_to_append_to)((left, right) => left addAll right.prepareAllPairs)
            case _ => list_to_append_to.addOne((list.head, list.tail.head))
          }
          prepareAllPairsRec(list.tail, list_to_append_to)
        }
      }
      prepareAllPairsRec(list, ListBuffer.empty[(LinkElem, LinkElem)])
    }
    //TODO а нафига я сделал такую специализированную функцию? надо чтобы брала все абсолютно пары, потом бы уже решали что то
    //а что не то
    def prepareConnectablePairs : List[(GearWheel, GearWheel)] = {
      @scala.annotation.tailrec
      def prepareConnectablePairsRec(list : List[LinkElem], list_to_append_to : ListBuffer[(GearWheel, GearWheel)]): List[(GearWheel,GearWheel)] ={
        def isWheel(a : LinkElem) : Boolean = a.isInstanceOf[GearWheel]
        def makeWheel(a : LinkElem) : GearWheel = a.asInstanceOf[GearWheel]
        if ( (list == Nil) || (list.isEmpty || list.tail.isEmpty)){
          list_to_append_to.toList
        }
        else {
          //сначала смотрим что справа
          //если сателлит - то берем сначала его нуль-колесо и сравниваем с нашим текущим колесом
          //если соединяемы - то все добавляем их кортеж в общую последовательность
          //возвращаем ее и последовательность, полученную от дальнейшего prepareConnectablePairs
          //если ранее был сателлит то на предыдущем этапе сначала линиаризируем цепочки сателлита и пускаем процесс по каждой из них
          val right = list.tail.head
          right match {
            case a : Satellite =>
              val lists = a.linearizeLists //getting linearized lists
              if (isWheel(list.head) && GearConnection.areConnectable((makeWheel(list.head), makeWheel(a.crowns(0).elems(0))))) {
                list_to_append_to.addOne((makeWheel(list.head), makeWheel(a.crowns(0).elems(0))))
                lists.foldLeft(list_to_append_to)((left, right) => left addAll right.prepareConnectablePairs)
              }
            case _ =>
              if (isWheel(list.head) && (isWheel(right)) && GearConnection.areConnectable((makeWheel(list.head), makeWheel(right)))){
                list_to_append_to.addOne((makeWheel(list.head), makeWheel(right)))
              }
          }
          prepareConnectablePairsRec(list.tail, list_to_append_to)
        }
      }
      prepareConnectablePairsRec(list, ListBuffer.empty[(GearWheel, GearWheel)])
    }
    def linearize : List[LinkElem] = {
      @scala.annotation.tailrec
      def linearizeRec(xs : List[LinkElem], list_to_append_to : ListBuffer[LinkElem]) : List[LinkElem] = {
        if (xs.isEmpty ) {
          list_to_append_to.toList
        }
        else{
          xs.head match {
            case sat : Satellite =>
              val lists = list_to_append_to.addAll(sat.linearize)
            case _ => list_to_append_to.addOne(xs.head)
          }
          linearizeRec(xs.tail, list_to_append_to)
        }
      }
      linearizeRec(list, ListBuffer.empty[LinkElem])
    }
  }
  implicit class LinkSeqOps(seq : LinkSeq) extends LinkSeqListOps(seq.elems)
  //------------------------------------------------
}