package planar_structure.mechanism.process.argument
import planar_structure.mechanism.mech2kh.WheelInfo
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

import scala.collection.mutable

/**
 *
 * @param z_min_max пара минимального и максимального значения числа зубьев для соответствующего колеса
 * @param k число сателлитов
 * @param u1h целевое передаточное отношение
 * @param eps_u1h требуемая точность передаточного отношения
 */
case class KinematicSynthesisArgs(z_min_max : Array[(Short, Short)], k : Byte, u1h : Float, eps_u1h : Int,
                                  mechanismType: MechanismType, carrierPosition: CarrierPosition,
                                  additional: Array[AdditionalWheelParams])

case class AdditionalWheelParams(ca : Float, ha: Float, x: Float, m : Float, alpha: Float, beta: Float)


case class MechanismCreatorArgs(carrierPosition: CarrierPosition, mechanismType: MechanismType,
                                wheelParams: List[WheelInfo], k : Byte)