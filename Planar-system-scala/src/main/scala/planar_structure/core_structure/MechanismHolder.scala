package planar_structure.core_structure

import planar_structure.core_structure.connections.{Connection, ConnectionImplicits, ConnectionMap}
import planar_structure.core_structure.links.{LinkHolderImplicits, WheelHolder}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class MechanismHolder(val linkSeq: LinkSeq, val controlMap : ConnectionMap)


object MechanismHolder extends LinkHolderImplicits{
  initMechanisms
  def initMechanisms : Unit ={
    creator_funcs.addOne(("One Row", () => {
      new MechanismHolder(
        linkSeq = LinkSeq(Input() :: ExternalWheel(WheelHolder.external) :: Satellite(mutable.HashMap(
          (0 -> LinkSeq(ExternalWheel(WheelHolder.external) :: InternalWheel(WheelHolder.internal) :: Nil))))
          :: Carrier() :: Output() :: Nil),
        controlMap = ConnectionMap(mutable.HashMap.empty[(LinkElem, LinkElem), Connection])
      )
    }))
  }
  //storage with access to mechanism creators by string
  lazy val creator_funcs : mutable.HashMap[String, () => MechanismHolder] = mutable.HashMap.empty[String, () => MechanismHolder]
}
