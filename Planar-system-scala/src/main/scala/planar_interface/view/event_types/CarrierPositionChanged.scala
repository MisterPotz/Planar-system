package planar_interface.view.event_types

import planar_structure.mechanism.types.CarrierPosition
import planar_interface.Event

case class CarrierPositionChanged(carrierPos : CarrierPosition)  extends Event
