package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.mech2kh.methods.{GeometricCommonMethods, GeometricMethodCarrierInputDecorator, GeometricMethodCarrierNeutral, GeometricMethodCarrierOutput}
import planar_structure.mechanism.{GearStructureCharacteristic, GearWheel, GeometricMethod, Mechanism}
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CodeGenerator}

object Geometric2KHMethod extends CodeGenerator{
  def apply(mechanism : Mechanism): GeometricMethod = {
    val decorated_with_carrier = mechanism.getCarrierPosition match {
      case CarrierInput =>
        new GeometricMethodCarrierInputDecorator(new GeometricCommonMethods(mechanism.gearStructureCharacteristic))
      case CarrierOutput =>
        new GeometricMethodCarrierOutput(new GeometricCommonMethods(mechanism.gearStructureCharacteristic))
      case CarrierNeutral =>
        new GeometricMethodCarrierNeutral(new GeometricCommonMethods(mechanism.gearStructureCharacteristic))
    }
    //TODO this one is changed - the method of the last decorator is written in the initial Common pack of methods
   /* decorated_with_carrier.methodable.getGearList.length match {
      case _ => new GeometricMethodNDecorator(decorated_with_carrier)
    }*/
    decorated_with_carrier
  }
 /* def apply(string : String) : GeometricMethod = {
    string.toFullType._2 match {
      case CarrierInput =>
        new GeometricMethodCarrierInputDecorator(new GeometricCommonMethods(mechanism.gearStructureCharacteristic))
      case CarrierOutput =>
        new GeometricMethodCarrierOutput(new GeometricCommonMethods(mechanism.gearStructureCharacteristic))
      case CarrierNeutral =>
        new GeometricMethodCarrierNeutral(new GeometricCommonMethods(mechanism.gearStructureCharacteristic))
    }
  }*/

}