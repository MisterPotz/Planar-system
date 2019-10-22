package planar_structure.core_structure

import planar_structure.{CarrierInput, CarrierOutput}
import planar_structure.core_structure.connections.{Connection, ConnectionChecker, ConnectionImplicits, GearConnection}
import planar_structure.core_structure.links.LinkHolderImplicits
import planar_structure.help_traits.BeautifulDebugOutput

import scala.reflect.ClassTag

case class Mechanism(mech_holder : MechanismHolder) extends ConnectionImplicits with BeautifulDebugOutput with MechanismImplicits {
  override def toStringShort: String ={
    "Mechanism:\n" +
      BeautifulDebugOutput.print(mech_holder.linkSeq.toStringShort + "\n----\nConnections of mechanism:\n"
        + BeautifulDebugOutput.print(mech_holder.controlMap.linearize(mech_holder.linkSeq).toStringShort))
  }
  def toStringBrief : String = {
    "Mechanism:\n" +
      BeautifulDebugOutput.print(mech_holder.linkSeq.toStringShort + "\n----\nConnections of mechanism:\n"
        + BeautifulDebugOutput.print(mech_holder.controlMap.linearize(mech_holder.linkSeq).toStringShort) + "\n----\nBrief Info:\n" +
      BeautifulDebugOutput.print(s"Links amount: ${mech_holder.getLinksAmount}\nConnections amount: ${mech_holder.getConnectionsAmount}"))
  }
  override def toString: String = toStringShort
}

trait MechanismImplicits{
  trait MechanismHolderOpsSubtype[+T]{
    type S <: T
    def installGearConnections() : S
    def installConnectionsWith[T <: LinkElem](installer : ConnectionChecker[T])(implicit tag : ClassTag[T]) : S
    def clearConnections() : S
  }
  trait MechanismCommonOps extends Implicits with ConnectionImplicits{
    def updateConnections() : Unit
    def getLinkOfType[T <: LinkElem](i : Int)(implicit tag : ClassTag[T]) : Option[T]
    def linearizeConnections : List[Connection]
    def linearizeElems : List[LinkElem]
    def linearizeElemsWith[T <: LinkElem](checker : Checker[T])(implicit tag : ClassTag[T]) : List[T]
    def getLink(i : Int) : LinkElem
    def getConnection(i : Int) : Connection
    def getLinksAmount : Int
    def getConnectionsAmount : Int
    def linearizePairs : List[(LinkElem, LinkElem)]
    def linearizePairsWith[T <: LinkElem](checker : ConnectionChecker[T])(implicit tag : ClassTag[T]) : List[(T,T)]
    def getLinksOfType[T <: LinkElem](implicit tag : ClassTag[T]) : List[T]
    def linearizeRaw : List[LinkElem]
    def getLinksRawOfType[T <: LinkElem](implicit tag : ClassTag[T]) : List[T]
    def getAllSatelliteConnections[T <: LinkElem](implicit tag : ClassTag[T]) : List[(T, T)]
    //def getBiggestWheelSatellite : GearWheel
    def findConnectionWith(elem : LinkElem) : Connection
    //TODO full mechanism reverse (so far it is not that necessary, the simpler approach is too just see if it's reversed
    //and build algorithms respectively
   // def reversed : Mechanism
    def isReversed : Boolean
  }
  implicit class MechanismHolderOps(mechanismHolder: MechanismHolder)
    extends MechanismHolderOpsSubtype[MechanismHolder] with MechanismCommonOps {
    type S = MechanismHolder
    override def installGearConnections() : MechanismHolder = {
      mechanismHolder.controlMap.installGearConnections(mechanismHolder.linkSeq)
      mechanismHolder
    }
    override def linearizeConnections : List[Connection] = mechanismHolder.controlMap.linearize(mechanismHolder.linkSeq)
    override def linearizeElems : List[LinkElem] = mechanismHolder.linkSeq.linearize

    override def getLink(i : Int) : LinkElem = mechanismHolder.linkSeq.getLink(i)
    override def getConnection(i : Int) : Connection = mechanismHolder.controlMap.linearize(mechanismHolder.linkSeq)(i)
    override def clearConnections() : MechanismHolder = {mechanismHolder.controlMap.holder.clear(); mechanismHolder}
    override def getLinksAmount : Int = {mechanismHolder.linkSeq.getBranchSize}
    override def getConnectionsAmount : Int = {mechanismHolder.controlMap.holder.knownSize}

    override def linearizePairs: List[(LinkElem, LinkElem)] = mechanismHolder.linkSeq.prepareAllPairs

    override def linearizePairsWith[T <: LinkElem](checker: ConnectionChecker[T])(implicit tag : ClassTag[T]) : List[(T, T)] =
      linearizePairs.filter((pair : (LinkElem, LinkElem)) => checker.canBeInstancedTo(pair)).map(pair => checker.reinstanceTo(pair))
        .filter(pair => checker.areConnectable(pair))

    override def installConnectionsWith[T <: LinkElem](installer: ConnectionChecker[T])(implicit tag: ClassTag[T]): MechanismHolder = {
      mechanismHolder.controlMap.installConnectionsWith(mechanismHolder.linkSeq, installer)
      mechanismHolder
    }
    override def getLinkOfType[T <: LinkElem](i: Int)(implicit tag : ClassTag[T]): Option[T] = {
      val links = linearizeElems
      val filterized = links.filter { (elem: LinkElem) =>
        elem match {
          case a: T => true
          case _ => false
        }
      }
      if (i < filterized.length & i >= 0) Some(filterized(i).asInstanceOf[T]) else None
    }

    override def updateConnections(): Unit = mechanismHolder.controlMap.updateConnections()

    override def getLinksOfType[T <: LinkElem](implicit tag: ClassTag[T]): List[T] = {
      linearizeElems.filter{
        case a : T => true
        case _ => false
      }.map(_.asInstanceOf[T])
    }

    override def linearizeElemsWith[T <: LinkElem](checker: Checker[T])(implicit tag: ClassTag[T]): List[T] = {
      mechanismHolder.linkSeq.linearize.filter(checker.canBeInstanced(_)).map(checker.reinstanceTo(_))
    }

    override def linearizeRaw: List[LinkElem] = mechanismHolder.linkSeq.elems

    override def getLinksRawOfType[T <: LinkElem](implicit tag: ClassTag[T]): List[T] = {
      linearizeRaw.filter{
        case a : T => true
        case _ => false
      }.map(_.asInstanceOf[T])
    }

    override def getAllSatelliteConnections[T <: LinkElem](implicit tag : ClassTag[T]): List[(T, T)] = {
      def check[Y <: LinkElem](p : (LinkElem, LinkElem))(implicit tag : ClassTag[Y]): Boolean ={
        p match {case p : (Y, Y) => true; case _ => false}
      }
      mechanismHolder.linkSeq.linearizeSatelliteFirstConnections
        .filter(check[T](_)(tag)).map(pair => pair.asInstanceOf[(T,T)])
    }

    /*override def getBiggestWheelSatellite: GearWheel = {
      val sat_pos = mechanismHolder.linkSeq.elems.indexWhere{case a : Satellite => true; case _ => false}
      mechanismHolder.linkSeq.elems(sat_pos).asInstanceOf[Satellite].holder.crowns.toList.map(_._2.elems(0).asInstanceOf[GearWheel]).maxBy(_.holder.d_a)
    }*/
    override def findConnectionWith(elem: LinkElem): Connection = {
      val key = mechanismHolder.controlMap.holder.toList.find(pair => ((pair._1._1 equals elem) || (pair._1._2 equals elem))).get._1
      mechanismHolder.controlMap.holder(key)
    }

    override def isReversed: Boolean = mechanismHolder.reversed
  }
  implicit class MechanismOps(mechanism: Mechanism) extends MechanismHolderOpsSubtype[Mechanism] with MechanismCommonOps {
    type S = Mechanism
    override def installGearConnections() : S = {mechanism.mech_holder.installGearConnections(); mechanism}
    override def clearConnections(): S  = {mechanism.mech_holder.clearConnections(); mechanism}
    override def linearizeConnections: List[Connection] = mechanism.mech_holder.linearizeConnections
    override def linearizeElems: List[LinkElem] = mechanism.mech_holder.linearizeElems
    override def getLink(i: Int): LinkElem = mechanism.mech_holder.getLink(i)
    override def getConnection(i: Int): Connection = mechanism.mech_holder.getConnection(i)
    override def getLinksAmount: Int = mechanism.mech_holder.getLinksAmount
    override def getConnectionsAmount: Int = mechanism.mech_holder.getConnectionsAmount

    override def linearizePairs: List[(LinkElem, LinkElem)] = mechanism.mech_holder.linearizePairs
    override def linearizePairsWith[T <: LinkElem](checker: ConnectionChecker[T])(implicit tag : ClassTag[T]) : List[(T, T)] =
      mechanism.mech_holder.linearizePairsWith(checker)

    override def installConnectionsWith[T <: LinkElem](installer: ConnectionChecker[T])(implicit tag: ClassTag[T]): Mechanism ={
      mechanism.mech_holder.installConnectionsWith(installer)
      mechanism
    }
    override def getLinkOfType[T <: LinkElem](i: Int)(implicit tag: ClassTag[T]): Option[T] = mechanism.mech_holder.getLinkOfType[T](i)

    override def updateConnections(): Unit = mechanism.mech_holder.updateConnections()

    override def getLinksOfType[T <: LinkElem](implicit tag: ClassTag[T]): List[T] = mechanism.mech_holder.getLinksOfType[T]

    override def linearizeElemsWith[T <: LinkElem](checker: Checker[T])(implicit tag: ClassTag[T]): List[T] = mechanism.mech_holder.linearizeElemsWith(checker)

    override def linearizeRaw: List[LinkElem] = mechanism.mech_holder.linearizeRaw

    override def getLinksRawOfType[T <: LinkElem](implicit tag: ClassTag[T]): List[T] = mechanism.mech_holder.getLinksRawOfType[T]

    override def getAllSatelliteConnections[T <: LinkElem](implicit tag: ClassTag[T]): List[(T, T)] = mechanism.mech_holder.getAllSatelliteConnections

    override def findConnectionWith(elem: LinkElem): Connection = mechanism.mech_holder.findConnectionWith(elem)

  //  override def getBiggestWheelSatellite: GearWheel = mechanism.mech_holder.getBiggestWheelSatellite
    override def isReversed: Boolean = mechanism.mech_holder.isReversed
  }
}


