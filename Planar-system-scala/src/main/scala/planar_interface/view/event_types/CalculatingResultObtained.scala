package planar_interface.view.event_types

import planar_interface.Event
import planar_structure.mechanism.process.report.{FullConditionCheck, KinematicSynthesisReport}

sealed trait CalculatingResultObtained extends Event
case class CalculatedKinematicForward(result : FullConditionCheck) extends CalculatingResultObtained
case class CalculatedKinematicSynthesis(result : KinematicSynthesisReport) extends CalculatingResultObtained

