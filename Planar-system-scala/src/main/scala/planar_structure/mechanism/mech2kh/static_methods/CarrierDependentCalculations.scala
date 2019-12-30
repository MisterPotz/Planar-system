package planar_structure.mechanism.mech2kh.static_methods

import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition, External1, ExternalExternal, ExternalInternal, InternalInternal, MechanismType}

trait CarrierDependentCalculationsInterface {
  def Uh4_1 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 / U4h_1(z_list,mechanismType)
  def U4h_1 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 - U41_h(z_list,mechanismType)
  def U41_h (z_list : List[Short],mechanismType : MechanismType): Float =
    1 / U14_h(z_list,mechanismType)
  def U1h_4 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 - U14_h(z_list,mechanismType)
  def Uh1_4 (z_list : List[Short],mechanismType : MechanismType): Float =
    1 /  U1h_4(z_list,mechanismType)
  def U14_h(z_list : List[Short],mechanismType : MechanismType): Float = {
    //TODO ASAP не учитывает тип зацепления
    //TODO  да он вообще не пойми как считает всё
    var modifier : Byte = 1
    mechanismType match {
      case ExternalExternal => modifier = 1
      case ExternalInternal => modifier = -1
      case InternalInternal => modifier = 1
      //case InternalExternal => modifier = -1
      case External1 => modifier = -1
      //case Internal1 => modifier = -1
    }
    z_list.length match {
      case 4 => {
        modifier * (z_list(1) * z_list(3)) / (z_list(0) * z_list(2)).toFloat
      }
      case 3 => {
        modifier * (z_list(2) / z_list(0).toFloat)
      }
    }
  }
  def calculateGearRatio(z_list: List[Short], carrierPos : CarrierPosition, mechanismType : MechanismType) : Float = {
    carrierPos match {
      case CarrierInput => Uh4_1(z_list, mechanismType)
      case CarrierOutput=> U1h_4(z_list,mechanismType)
      case CarrierNeutral => U14_h(z_list,mechanismType)
  }
  }

}


object RatioEfficientCalculator{
  def calculateMiniRatio(z1_list : List[Short], z2_list: List[Short]) : List[Float] = {
    z2_list.flatMap(z2 => z1_list.map(z1 => {
      z2 / z1.toFloat
    }))
  }
  def calculateMiniRatio(z1 : Short, z2: Short) : Float = {
    z2 / z1.toFloat
  }
  def calculateFullRatio(u1_raw : Float, u2_raw : Float, mechanismType: MechanismType) : Float = {
    var modifier : Byte = 1
    mechanismType match {
      case ExternalExternal => modifier = 1
      case ExternalInternal => modifier = -1
      case InternalInternal => modifier = 1
     // case InternalExternal => modifier = -1
      case External1 => modifier = -1
     // case Internal1 => modifier = -1
    }
    modifier * u1_raw * u2_raw
  }
  def Uh4_1 (u1_raw : Float, u2_raw : Float, mechanismType: MechanismType): Float =
    1 / U4h_1(u1_raw , u2_raw, mechanismType)
  def U4h_1 (u1_raw : Float, u2_raw : Float, mechanismType: MechanismType): Float =
    1 - U41_h(u1_raw , u2_raw, mechanismType)
  def U41_h (u1_raw : Float, u2_raw : Float, mechanismType: MechanismType): Float =
    1 / U14_h(u1_raw , u2_raw, mechanismType)
  def U1h_4 (u1_raw : Float, u2_raw : Float, mechanismType: MechanismType): Float =
    1 - U14_h(u1_raw , u2_raw, mechanismType)
  def Uh1_4 (u1_raw : Float, u2_raw : Float, mechanismType: MechanismType): Float =
    1 /  U1h_4(u1_raw , u2_raw, mechanismType)
  def U14_h(u1_raw : Float, u2_raw : Float, mechanismType: MechanismType): Float = {
    //TODO ASAP не учитывает тип зацепления
    //TODO  да он вообще не пойми как считает всё
    calculateFullRatio(u1_raw , u2_raw, mechanismType)
  }
  def calculateGearRatio(u1_raw : Float, u2_raw : Float, carrierPos : CarrierPosition,
  mechanismType: MechanismType) : Float = {
    carrierPos match {
      case CarrierInput => Uh4_1(u1_raw , u2_raw, mechanismType)
      case CarrierOutput=> U1h_4(u1_raw , u2_raw, mechanismType)
      case CarrierNeutral => U14_h(u1_raw , u2_raw, mechanismType)
    }
  }
}

object CarrierDependentCalculations extends CarrierDependentCalculationsInterface

object GearRatioTest extends App{
  val first = Range(17, 100)
  val second = Range(17,100)
  val third = Range(17,100)
  val fourth = Range(81,200)
  val first_stage_ratio = first.flatMap(z1 => second.map(z2 => {
    (z1, z2, RatioEfficientCalculator.calculateMiniRatio(z1.toShort,z2.toShort))
  }))
  val second_stage_ratio = third.flatMap(z3 => fourth.map(z4 => {
    (z3, z4, RatioEfficientCalculator.calculateMiniRatio(z3.toShort,z4.toShort))
  }))
  first_stage_ratio.slice(0,10).foreach(println(_))
  println("_-------------------_")
  second_stage_ratio.slice(0,10).foreach(println(_))
  val ratio = RatioEfficientCalculator.calculateGearRatio(first_stage_ratio(0)._3, second_stage_ratio(0)._3,
    CarrierOutput, ExternalInternal)
  println(ratio)
}