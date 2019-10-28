package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.GearWheel

import scala.collection.mutable.ListBuffer

package object mech2kh_EE {
  class Mechanism2kh_EE extends Mechanism2KH{
    override val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = null //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = new GearStructureCharacteristic {
        val gear_list : List[GearWheel] = {
          val buff : ListBuffer[GearWheel] = ListBuffer.empty[GearWheel]
          for (i <- Range(0,4)) {
            buff.addOne(new ExternalWheel())
          }
          buff.toList
        }
        val gear_connection_list : List[GearConnection] = {
          preparePairsForConnections.map{ a =>
            new GearConnection(a._1, a._2)
          }
        }
        override def getGearList: List[GearWheel] = {
          gear_list
        }
        override def getGearConnectionList: List[GearConnection] = gear_connection_list
        //satellite gears are the ones from 1 to 2 including
        override def getSatelliteGears: List[GearWheel] = gear_list.slice(1,3)
      }
    }
    override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }

}
