package planar_interface.view.event_types

import planar_interface.Event
import planar_structure.mechanism.types.MechanismType

case class MechanismChangedEvent(new_mech : MechanismType) extends Event
