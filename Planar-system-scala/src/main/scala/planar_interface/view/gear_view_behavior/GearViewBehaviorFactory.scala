package planar_interface.view.gear_view_behavior

import planar_structure.mechanism.types.ProcessType
trait AbstractGearViewBehaviorFactory{
  def apply(behavior_mode : String) : GearViewBehavior
}

object GearViewBehaviorFactory extends AbstractGearViewBehaviorFactory {
  override def apply(behavior_mode : String) : GearViewBehavior = {
    behavior_mode match {
      case ProcessType.KINEMATIC_ANALYSIS_FORWARD => new KinematicForwardBehavior
      case ProcessType.KINEMATIC_SYNTHESIS => new KinematicSynthesisBehavior
    //TODO  case ProcessType.KINEMATIC_SYNTHESIS => new
    }
  }
}
