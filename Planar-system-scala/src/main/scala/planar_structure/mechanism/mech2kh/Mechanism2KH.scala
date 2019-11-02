package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition, Characteristic, ExternalWheel, GearStructureCharacteristic, GearWheel, GeometricMethod, InternalWheel, Mechanism}

import scala.collection.mutable.ListBuffer


abstract class Mechanism2KH extends Mechanism

//used to understand what wheels are present in the mechanism
sealed trait MechanismType;
case object ExternalInternal extends MechanismType
case object InternalExternal extends MechanismType
case object ExternalExternal extends MechanismType
case object InternalInternal extends MechanismType
case object External1 extends MechanismType //простейший тип планетарного механизма
case object Internal1 extends MechanismType //такой же простой только наоборот

//constructor for proper gear structure characteristics
object GearStructure2KHCharacteristic{
  //TODO make decorators for this things
  trait GearStructure2KH_4Wheels_Characteristic extends GearStructureCharacteristic{
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,3)
    override def preparePairsForConnections: List[(GearWheel, GearWheel)] = {
      (getGearList(0), getGearList(1)) :: (getGearList(2), getGearList(3)) :: Nil;
    }
  }
  trait GearStructure2KH_3Wheels_Characteristis extends GearStructureCharacteristic{
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,2)
    override def preparePairsForConnections: List[(GearWheel, GearWheel)] = {
      (getGearList(0), getGearList(1)) :: (getGearList(1), getGearList(2)) :: Nil;
    }
  }
  def apply(mechanismType: MechanismType, carrierPosition: CarrierPosition = CarrierOutput): GearStructureCharacteristic ={
    mechanismType match {
      case ExternalExternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = new ExternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  ExternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition

      }
      case InternalExternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = new InternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  ExternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
      case ExternalInternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = new ExternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  InternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
      case InternalInternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = new InternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  InternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
      case External1 => new GearStructure2KH_3Wheels_Characteristis {
        override protected val gears: List[GearWheel] = new ExternalWheel() :: new ExternalWheel() :: new InternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
      case Internal1 => new GearStructure2KH_3Wheels_Characteristis {
        override protected val gears: List[GearWheel] = new InternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
    }
  }
}


object Geometric2KHMethod{
  class GeometricCommonMethods(meth : GearStructureCharacteristic) extends GeometricMethod{
    override def getGearRatioCarrierStopped: Double = {
      println("Common GearRatioCarrierStopped")
      methodable.getGearConnectionList.foldLeft(1.0)((f, s) => {val res = f * s.U; println(s"latest U: ${res}"); res}) //просто считает как обычные зацепления весь механизм с остановленным водилом
    } //from a to the last one with h as stopped
    override def noPruningOnGear(i: Int): Boolean = {
      methodable.getGear(i).holder.noPruning
    }
    override def noPruningOnAll: Boolean = { //проверяет все колеса на предмет подрезания
      @scala.annotation.tailrec
      def rec_no_pruning(xs : List[GearWheel]) : Boolean = {
        if (xs.isEmpty){
          true
        } else{
          if (!xs.head.holder.noPruning){
            false
          } else{
            rec_no_pruning(xs.tail)
          }
        }
      }
      rec_no_pruning(methodable.getGearList)
    }
    override def assemblyCondition: Boolean = {
      val z = methodable.getSatelliteGears(0).holder.z
      val U1H = getGearRatio
      val k = methodable.amount_of_satellites
      @scala.annotation.tailrec
      def calc(p : Int): Boolean ={
        if (p == 6) {
          true
        } else{
          val res : Double = U1H * z / k * (1 + p * k)
          if (res - res.round >= 0.001){
            false
          }else{
            calc(p+1)
          }
        }
      }
      calc(1)
    }
    override def minimalSize(a: List[Characteristic]): Characteristic = {
      val list : List[GearStructureCharacteristic] = a.map{_.asInstanceOf[GearStructureCharacteristic]}.appended(methodable)
      list.minBy(_.getMaxRw)
    }
    override def alignmentCondition: Boolean = {
      val res : Double = methodable.getGearConnectionList(0).connectionCalculationBehavior.aw - methodable.getGearConnectionList(1).connectionCalculationBehavior.aw
      if (res <= 0.001) true else false
    }
    override var methodable: GearStructureCharacteristic = meth
    override def getGearRatio: Double = 0.0 //carrier decorated
    override def getGearRatioBackwards: Double = 0.0 //carrier decorated
    override def neighborhoodCondition: Boolean = true //TODO must be diversed in Decorator on !amount of satellites!
    override def interferenceCondition(i : Int): Boolean = {
      methodable.getGearConnectionList(i).connectionCalculationBehavior.interference
    }
    override def minimalSizeComparingTo(a: List[Characteristic]): Boolean = {
      if (minimalSize(a).asInstanceOf[GearStructureCharacteristic].getMaxRw >= methodable.getMaxRw){
        true
      } else false
    }
  }
  //abstract decorator class for GeometricMethod, extends GeometricCommonMethod along with the GeometricMethod
  abstract class GeometricMethodDecorator(decoratable : GeometricMethod) extends GeometricMethod {
    val decoratable_object : GeometricMethod = decoratable
    override var methodable: GearStructureCharacteristic = decoratable_object.methodable
    override def getGearRatio: Double = decoratable_object.getGearRatio
    override def getGearRatioBackwards: Double = decoratable_object.getGearRatioBackwards
    override def getGearRatioCarrierStopped: Double = decoratable_object.getGearRatioCarrierStopped
    override def alignmentCondition: Boolean = decoratable_object.alignmentCondition
    override def assemblyCondition: Boolean = decoratable_object.assemblyCondition
    override def interferenceCondition(i : Int): Boolean = decoratable_object.interferenceCondition(i)
    override def neighborhoodCondition: Boolean = decoratable_object.neighborhoodCondition
    override def noPruningOnGear(i: Int): Boolean = decoratable_object.noPruningOnGear(i)
    override def noPruningOnAll: Boolean = decoratable_object.noPruningOnAll
    //override def overlapFactorOk: Boolean = decoratable_object.overlapFactorOk
    override def minimalSize(a: List[Characteristic]): Characteristic = decoratable_object.minimalSize(a)
    override def minimalSizeComparingTo(a: List[Characteristic]): Boolean = decoratable_object.minimalSizeComparingTo(a)

  }
  trait U{
    this : GeometricMethodDecorator =>
    def Uh4_1 : Double = 1 / U4h_1
    def U4h_1 : Double = 1 - U41_h
    def U41_h : Double = 1 / U14_h
    def U14_h : Double = getGearRatioCarrierStopped
    def U1h_4 : Double = 1 - U14_h
    def Uh1_4 : Double = 1 /  U1h_4
  }
  class GeometricMethodCarrierInputDecorator(dec : GeometricMethod) extends GeometricMethodDecorator(dec) with U{
    override def getGearRatioBackwards: Double = {val res = U4h_1; println(s"CarrierInputDecorator backwards ratio: ${res}"); res}
    override def getGearRatio: Double = Uh4_1
  }
  class GeometricMethodCarrierOutput(dec : GeometricMethod) extends GeometricMethodDecorator(dec) with U{
    override def getGearRatioBackwards: Double = Uh1_4 //TODO уточнить функции
    override def getGearRatio: Double = U1h_4
  }
  class GeometricMethodCarrierNeutral(dec : GeometricMethod) extends GeometricMethodDecorator(dec) with U{
    override def getGearRatioBackwards: Double = U41_h
    override def getGearRatio: Double = U14_h
  }
/*  class GeometricMethod3Decorator(dec : GeometricMethod) extends  GeometricMethodDecorator(dec){
    //  override def
    override def neighborhoodCondition: Boolean = true
  }*/
  class GeometricMethodNDecorator(dec : GeometricMethod) extends  GeometricMethodDecorator(dec){
    override def neighborhoodCondition: Boolean = {
       val z : List[Double] = methodable.getGearList.slice(0,3).map(_.holder.z)
      val gears : List[GearWheel] = methodable.getGearList
      val alpha_t : Double = methodable.getGear(0).holder.alpha_t
      val alpha_tw : Double = methodable.getGearConnectionList(0).connectionCalculationBehavior.alpha_tw
      val k : Double = methodable.amount_of_satellites
      if (k == 1.0){
        true
      } else {
        val A : Double = (z(0) + z(1)) * math.cos(alpha_t ) / math.cos(alpha_tw) * math.sin(math.Pi / k);
        val mt2 : Double = gears(1).holder.mt	// 1й ряд
        val xt2 : Double =  gears(1).holder.xt
        val hta2 : Double = gears(1).holder.hta
        val alpha_t2 : Double  = gears(1).holder.alpha_t
        val alpha_tw2 : Double = methodable.getGearConnectionList(0).connectionCalculationBehavior.alpha_tw
        val y2 = z(1) * (math.cos(alpha_t2) / math.cos(alpha_tw2) - 1);
        val dy2 = 2 * xt2 - y2;

        val mt3 : Double = gears(2).holder.mt		// 2й ряд
        val xt3 : Double = gears(2).holder.xt
        val hta3 : Double = gears(2).holder.hta
        val alpha_t3 : Double = gears(2).holder.alpha_t
        val alpha_tw3 : Double = methodable.getGearConnectionList(1).connectionCalculationBehavior.alpha_tw
        val y3 = z(2) * (math.cos(alpha_t3) / math.cos(alpha_tw3) - 1)
        val dy3 = 2 * xt3 - y3;
        val B2 = (z(1) / 2 + xt2 + hta2 - dy2);
        val B3 = (z(2) / 2 + xt3 + hta3 - dy3);
        if (B2 >= B3)
        {
          (A > B2);
        }
        else
        {
          (A > B3);
        }
      }
    }
  }
  def apply(gearGeometricCharacteristic: GearStructureCharacteristic): GeometricMethod = {
      val decorated_with_carrier = gearGeometricCharacteristic.info match {
        case CarrierInput => new GeometricMethodCarrierInputDecorator(new GeometricCommonMethods(gearGeometricCharacteristic))
        case CarrierOutput => new GeometricMethodCarrierOutput(new GeometricCommonMethods(gearGeometricCharacteristic))
        case CarrierNeutral => new GeometricMethodCarrierNeutral(new GeometricCommonMethods(gearGeometricCharacteristic))
      }
      decorated_with_carrier.methodable.getGearList.length match {
        case _ => new GeometricMethodNDecorator(decorated_with_carrier)
      }
  }

}