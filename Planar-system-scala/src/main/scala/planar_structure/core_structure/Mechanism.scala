package planar_structure.core_structure

import planar_structure.core_structure.connections.{Connection, ConnectionChecker, ConnectionImplicits}
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
    def linearizeConnections : List[Connection]
    def linearizeElems : List[LinkElem]
    def getLink(i : Int) : LinkElem
    def getConnection(i : Int) : Connection
    def getLinksAmount : Int
    def getConnectionsAmount : Int
    def linearizePairs : List[(LinkElem, LinkElem)]
    def linearizePairsWith[T <: LinkElem](checker : ConnectionChecker[T])(implicit tag : ClassTag[T]) : List[(T,T)]
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
  }
}


