package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import planar_structure.mechanism.process.argument.AdditionalWheelParams

case class KinematicSynthesisArgsPartial(z_min_max : Array[(Short, Short)], k : Byte, u1h : Float, eps_u1h : Int,
                                         additionalWheelParams: Array[AdditionalWheelParams])
