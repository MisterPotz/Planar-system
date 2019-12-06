package planar_structure.mechanism.common_mechanisms

sealed trait PlanarMechanismType
case object _2K_H extends PlanarMechanismType
case object  _3K extends PlanarMechanismType


sealed trait MECHANISM_FULL_CLASSIFIER{
  def U_direct_H(targetU : Float) : Float
  val wheels : Int
}
sealed trait A_CLASSIFIER extends  MECHANISM_FULL_CLASSIFIER{
  override val wheels: Int = 3
}
case object A_AH_B extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = {
    1 - targetU
  }
}
case object A_AB_H extends A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = targetU
}
case object A_BH_A extends  A_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 / (1 - 1 / targetU)
}

sealed trait B_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER{
  override val wheels: Int = 4
}
case object B_AH_B extends B_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 - targetU
}

sealed trait C_CLASSIFIER extends MECHANISM_FULL_CLASSIFIER{
  override val wheels: Int = 4
}
case object C_HB_E extends C_CLASSIFIER {
  override def U_direct_H(targetU: Float): Float = 1 / (1 - 1 / targetU)
}


/**
 *
 * @param U передаточное отношение механизма. т.к. типы механизмов разные
 *          то здесь U принимается таким, каким оно обычно приводится в справочниках для того или иного типа механизма,
 *          то есть может означать как п.о. при остановленном звене b, так и при остановленном звене а.
 * @param KPD типичная для данного типа механизма область определения передаточного отношения
 * @param TYPE тип планетарной передачи
 * @param CLASSIFIER классификатор по Кудрявцеву (табл. 1.1)
 */
case class CommonMechanism(
                          U: (Float, Float),
                          KPD : (Float, Float),
                          TYPE : PlanarMechanismType,
                          CLASSIFIER : MECHANISM_FULL_CLASSIFIER
                          )


object CommonMechanismCharacteristics {
  val COMMON_MECHANISMS = Array(
    CommonMechanism((3,9),(0.97f,0.99f),_2K_H,A_AH_B),
    CommonMechanism((1.13f,1.5f),(0.99f,0.996f),_2K_H, A_BH_A),
    CommonMechanism((-2,-8),(0.96f,0.985f),_2K_H, A_AB_H),
    CommonMechanism((7,16),(0.97f,0.99f),_2K_H, B_AH_B),
    CommonMechanism((8,30),(0.75f,0.8f),_2K_H, C_HB_E),
    CommonMechanism((25,300),(0.4f,0.9f),_2K_H, C_HB_E)
  )
}
