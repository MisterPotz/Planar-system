package planar_structure

sealed trait MechanismType;
trait CarrierOutput extends MechanismType;
trait CarrierInput extends MechanismType;