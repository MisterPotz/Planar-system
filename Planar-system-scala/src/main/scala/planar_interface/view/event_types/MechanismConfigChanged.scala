package planar_interface.view.event_types

import planar_interface.Event
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

case class MechanismConfigChanged(mechanismType : MechanismType, carrierPos : CarrierPosition ) extends Event
