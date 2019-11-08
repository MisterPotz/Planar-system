package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.mech2kh.methods.{GeometricCommonMethods, GeometricMethodCarrierInputDecorator, GeometricMethodCarrierNeutral, GeometricMethodCarrierOutput}
import planar_structure.mechanism.{Characteristic, GearStructureCharacteristic, GearWheel, GeometricMethod}
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput}

object Geometric2KHMethod{
  def apply(gearGeometricCharacteristic: GearStructureCharacteristic): GeometricMethod = {
    val decorated_with_carrier = gearGeometricCharacteristic.info match {
      case CarrierInput => new GeometricMethodCarrierInputDecorator(new GeometricCommonMethods(gearGeometricCharacteristic))
      case CarrierOutput => new GeometricMethodCarrierOutput(new GeometricCommonMethods(gearGeometricCharacteristic))
      case CarrierNeutral => new GeometricMethodCarrierNeutral(new GeometricCommonMethods(gearGeometricCharacteristic))
    }
    //TODO this one is changed - the method of the last decorator is written in the initial Common pack of methods
   /* decorated_with_carrier.methodable.getGearList.length match {
      case _ => new GeometricMethodNDecorator(decorated_with_carrier)
    }*/
    decorated_with_carrier
  }

}