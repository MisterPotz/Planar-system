package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.{External1, ExternalExternal, ExternalInternal, GearStructure2KHCharacteristic, Geometric2KHMethod, Internal1, InternalExternal, InternalInternal, Mechanism2KH}
import planar_structure.mechanism.GearWheel

import scala.collection.mutable.ListBuffer

package object mech2kh_EE {
  class Mechanism2kh_EE(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override  val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(ExternalExternal, carrierPosition)
    }
    override val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = Geometric2KHMethod(characteristics.gearStructureCharacteristic)
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    ///override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}

package object mech2kh_II {
  class Mechanism2kh_II(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override lazy val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = Geometric2KHMethod(characteristics.gearStructureCharacteristic) //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override lazy val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(InternalInternal, carrierPosition)
    }
    ///override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}

package object mech2kh_EI {
  class Mechanism2kh_EI(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override lazy val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = Geometric2KHMethod(characteristics.gearStructureCharacteristic) //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override lazy val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(ExternalInternal,carrierPosition)
    }
  //  override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}

package object mech2kh_IE{
  class Mechanism2kh_IE(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override lazy val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = Geometric2KHMethod(characteristics.gearStructureCharacteristic) //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override lazy val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(InternalExternal,carrierPosition)
    }
    //override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}

package object mech2kh_E1{
  class Mechanism2kh_E1(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override lazy val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = Geometric2KHMethod(characteristics.gearStructureCharacteristic)//TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override lazy val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(External1,carrierPosition)
    }
    //override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}
package object mech2kh_I1{
  class Mechanism2kh_I1(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override lazy val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = Geometric2KHMethod(characteristics.gearStructureCharacteristic)//TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override lazy val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(Internal1,carrierPosition)
    }
    //override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}
trait MechanismFactory{
  def apply(code : String) : Mechanism
  def safeApply(code : String) : Either[Boolean, Mechanism]
}
//e.g. form of code: "ExternalExternal_CarrierInput"
object Mechanism2KH extends MechanismFactory {
  override def apply(code : String): Mechanism ={
    val carrier = code.split("_")(1) match {
      case "CarrierInput" => CarrierInput
      case "CarrierOutput" => CarrierOutput
      case "CarrierNeutral" => CarrierNeutral
    }
    code.split("_")(0) match {
      case "ExternalExternal" => new mech2kh_EE.Mechanism2kh_EE(carrier)
        //_----------------------------
      case "ExternalInternal" => new mech2kh_EI.Mechanism2kh_EI(carrier)
        //-------------------------------
      case "InternalInternal" => new mech2kh_II.Mechanism2kh_II(carrier)
        //-----------------------------------
      case "InternalExternal" => new mech2kh_IE.Mechanism2kh_IE(carrier)
        //--------------------------------
      case "External1" => new mech2kh_E1.Mechanism2kh_E1(carrier)
        //----------------------
      case "Internal1" => new mech2kh_I1.Mechanism2kh_I1(carrier)

    }
  }

  override def safeApply(code: String): Either[Boolean, Mechanism] = {
    try {
      Right(apply(code))
    }catch {
      case _ : Exception => Left(false)
    }
  }
}