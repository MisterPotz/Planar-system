package planar_structure.old.core_structure

import planar_structure.old.core_structure.connections.{Connection, ConnectionImplicits, ConnectionMap}
import planar_structure.old.core_structure.links.{LinkHolderImplicits, WheelHolder, WheelPositionHolder}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//reverse - if true than the input is a carrier, otherwise the carrier is an output
class MechanismHolder(val linkSeq: LinkSeq, val controlMap : ConnectionMap, val satellite_amount : Int = 3, val reversed : Boolean = false)


object MechanismHolder extends LinkHolderImplicits{
  initMechanisms
  def initMechanisms : Unit ={
    creator_funcs.addOne(("One Row", () => {
      new MechanismHolder(
        linkSeq = LinkSeq(Input() :: ExternalWheel(WheelHolder.external, WheelPositionHolder.lower) :: Satellite(mutable.HashMap(
          (0 -> LinkSeq(ExternalWheel(WheelHolder.external, WheelPositionHolder.higher) :: InternalWheel(WheelHolder.internal, WheelPositionHolder.higher) :: Nil))))
          :: Output() :: Nil),
        controlMap = ConnectionMap.empty
      )
    }))
    creator_funcs.addOne{
      ("Two Row EI", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: ExternalWheel(WheelHolder.external, WheelPositionHolder.default) ::
          Satellite(mutable.HashMap(
            (0 -> LinkSeq(ExternalWheel(WheelHolder.external, WheelPositionHolder.higher) :: Nil)),
            (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: InternalWheel(WheelHolder.internal, WheelPositionHolder.lower) :: Nil))
          )) ::
          Output() :: Nil
          ),
          controlMap = ConnectionMap.empty
        )
      })
    }
    creator_funcs.addOne{
      ("Two Row IE", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: InternalWheel(WheelHolder.internal) ::
            Satellite(mutable.HashMap(
              (0 -> LinkSeq(ExternalWheel(WheelHolder.external) :: Nil)),
              (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: ExternalWheel(WheelHolder.external) :: Nil))
            )) ::
            Output() :: Nil
          ),
          controlMap = ConnectionMap.empty
        )
      })}
    creator_funcs.addOne{
      ("Two Row IECI", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: InternalWheel(WheelHolder.internal) ::
            Satellite(mutable.HashMap(
              (0 -> LinkSeq(ExternalWheel(WheelHolder.external) :: Nil)),
              (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: ExternalWheel(WheelHolder.external) :: Nil))
            )) ::
            Output() :: Nil
          ),
          controlMap = ConnectionMap.empty,
          reversed = true
        )
      })}
    creator_funcs.addOne{
      ("Two Row II", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: InternalWheel(WheelHolder.internal) ::
            Satellite(mutable.HashMap(
              (0 -> LinkSeq(ExternalWheel(WheelHolder.external, WheelPositionHolder.higher) :: Nil)),
              (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: InternalWheel(WheelHolder.internal, WheelPositionHolder.lower) :: Nil))
            )) ::
            Output() :: Nil
          ),
          controlMap = ConnectionMap.empty
        )
      })}
    //TODO  унаследовать от айнганга и аусганга нужные элементы
    creator_funcs.addOne{
      ("Two Row IICI", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: InternalWheel(WheelHolder.internal) ::
            Satellite(mutable.HashMap(
              (0 -> LinkSeq(ExternalWheel(WheelHolder.external, WheelPositionHolder.higher) :: Nil)),
              (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: InternalWheel(WheelHolder.internal, WheelPositionHolder.lower) :: Nil))
            )) ::
            Output() :: Nil
          ),
          controlMap = ConnectionMap.empty,
          reversed = true
        )
      })}
    creator_funcs.addOne{
      ("Two Row Test", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: InternalWheel(WheelHolder.internal) ::
            Satellite(mutable.HashMap(
              (0 -> LinkSeq(InternalWheel(WheelHolder.internal) :: Nil)),
              (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: InternalWheel(WheelHolder.internal) :: Nil))
            )) ::
            Output() :: Nil
          ),
          controlMap = ConnectionMap.empty
        )
      })}
    creator_funcs.addOne{
      ("Two Row EE", () => {
        new MechanismHolder(
          linkSeq = LinkSeq(Input() :: ExternalWheel(WheelHolder.external)  ::
            Satellite(mutable.HashMap(
              (0 -> LinkSeq(ExternalWheel(WheelHolder.external, WheelPositionHolder.higher)  :: Nil)),
              (1 -> LinkSeq(ExternalWheel(WheelHolder.external) :: ExternalWheel(WheelHolder.external, WheelPositionHolder.lower)  :: Nil))
            )) ::
            Output() :: Nil
          ),
          controlMap = ConnectionMap.empty
        )
      })}
    }

  //storage with access to mechanism creators by string
  lazy val creator_funcs : mutable.HashMap[String, () => MechanismHolder] = mutable.HashMap.empty[String, () => MechanismHolder]
}
