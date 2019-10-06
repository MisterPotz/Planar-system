package planar_structure.core_structure

import planar_structure.core_structure.connections.{Connection, ConnectionImplicits}
import planar_structure.core_structure.links.LinkHolderImplicits
import planar_structure.help_traits.BeautifulDebugOutput

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
    def installConnections() : S
    def clearConnections() : S
  }
  trait MechanismCommonOps extends Implicits with ConnectionImplicits{
    def linearizeConnections : List[Connection]
    def linearizeElems : List[LinkElem]
    def getLink(i : Int) : LinkElem
    def getConnection(i : Int) : Connection
    def getLinksAmount : Int
    def getConnectionsAmount : Int
  }
  implicit class MechanismHolderOps(mechanismHolder: MechanismHolder)
    extends MechanismHolderOpsSubtype[MechanismHolder] with MechanismCommonOps {
    type S = MechanismHolder
    override def installConnections() : MechanismHolder = {
      mechanismHolder.controlMap.installConnections(mechanismHolder.linkSeq)
      mechanismHolder
    }
    override def linearizeConnections : List[Connection] = mechanismHolder.controlMap.linearize(mechanismHolder.linkSeq)
    override def linearizeElems : List[LinkElem] = mechanismHolder.linkSeq.linearize
    override def getLink(i : Int) : LinkElem = mechanismHolder.linkSeq.getLink(i)
    override def getConnection(i : Int) : Connection = mechanismHolder.controlMap.linearize(mechanismHolder.linkSeq)(i)
    override def clearConnections() : MechanismHolder = {mechanismHolder.controlMap.holder.clear(); mechanismHolder}
    override def getLinksAmount : Int = {mechanismHolder.linkSeq.getBranchSize}
    override def getConnectionsAmount : Int = {mechanismHolder.controlMap.holder.knownSize}
  }
  implicit class MechanismOps(mechanism: Mechanism) extends MechanismHolderOpsSubtype[Mechanism] with MechanismCommonOps {
    type S = Mechanism
    override def installConnections() : S = {mechanism.mech_holder.installConnections(); mechanism}
    override def clearConnections(): S  = {mechanism.mech_holder.clearConnections(); mechanism}
    override def linearizeConnections: List[Connection] = mechanism.mech_holder.linearizeConnections
    override def linearizeElems: List[LinkElem] = mechanism.mech_holder.linearizeElems
    override def getLink(i: Int): LinkElem = mechanism.mech_holder.getLink(i)
    override def getConnection(i: Int): Connection = mechanism.mech_holder.getConnection(i)
    override def getLinksAmount: Int = mechanism.mech_holder.getLinksAmount
    override def getConnectionsAmount: Int = mechanism.mech_holder.getConnectionsAmount
  }
}


