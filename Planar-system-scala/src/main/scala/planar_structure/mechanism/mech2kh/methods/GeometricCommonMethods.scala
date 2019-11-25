package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.process.report.FullConditionCheck
import planar_structure.mechanism.{GearConnection, GearStructureCharacteristic, GearWheel, GeometricMethod}

//these methods are common for every type of mechanism
class GeometricCommonMethods(meth : GearStructureCharacteristic) extends GeometricMethod{
  override def getGearRatioCarrierStopped: Double = {
   // println("Common GearRatioCarrierStopped")
    structure.getGearConnectionList.foldLeft(1.0)((f, s) => {val res = f * s.U; //println(s"latest U: ${res}");
      res}) //просто считает как обычные зацепления весь механизм с остановленным водилом
  } //from a to the last one with h as stopped
  override def noPruningOnGear(i: Int): Boolean = {
    structure.getGearList(i).holder.noPruning
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
    rec_no_pruning(structure.getGearList)
  }
  override def assemblyCondition: Boolean = {
    val z = structure.getGearList(0).holder.z.toFloat
    val U1H = U1h_4
    //println(s"Gear ratio in assembly condition: ${U1H}")
    val k = structure.storage.k.toFloat
    @scala.annotation.tailrec
    def calc(p : Int): Boolean ={
      if (p == 6) {
        true
      } else{
        val res : Float = (U1H * z.toFloat / k.toFloat * (1 + p * k)).toFloat
        val res_rounded : Float = res.round
        if (math.abs(res - res_rounded) >= 0.001f){
          false
        }else{
          calc(p+1)
        }
      }
    }
    calc(1)
  }
  override def minimalSize(a: List[GearStructureCharacteristic]): GearStructureCharacteristic = {
    val list : List[GearStructureCharacteristic] = a.map{_.asInstanceOf[GearStructureCharacteristic]}.appended(structure)
    list.minBy(_.getMaxRw)
  }
  override def alignmentCondition: Boolean = {
    val res : Double = structure.getGearConnectionList(0).connectionCalculationBehavior.aw - structure.getGearConnectionList(1).connectionCalculationBehavior.aw
    if (math.abs(res) <= 0.001) true else false
  }
  override var structure: GearStructureCharacteristic = meth
  override def getGearRatio: Double = outerMethod.getGearRatio  //carrier decorated
  override def getGearRatioBackwards: Double = outerMethod.getGearRatioBackwards  //carrier decorated
  override def interferenceCondition(i : Int): Boolean = {
    structure.getGearConnectionList(i).connectionCalculationBehavior.interference
  }
  override def minimalSizeComparingTo(a: List[GearStructureCharacteristic]): Boolean = {
    if (minimalSize(a).asInstanceOf[GearStructureCharacteristic].getMaxRw >= structure.getMaxRw){
      true
    } else false
  }

  override def neighborhoodCondition: Boolean = {
    /*val z : List[Float] = structure.getGearList.slice(0,3).map(_.holder.z.toFloat)
    val gears : List[GearWheel] = structure.getGearList
    val alpha_t : Float = structure.getGearList(0).holder.alpha_t
    val alpha_tw : Float = structure.getGearConnectionList(0).connectionCalculationBehavior.alpha_tw
    val k : Float = structure.storage.k
    if (k == 1.0){
      true
    } else {
      val A : Float = ((z(0) + z(1)) * math.cos(alpha_t ) / math.cos(alpha_tw) * math.sin(math.Pi / k.toFloat)).toFloat
      val mt2 : Float = gears(1).holder.mt	// 1й ряд
      val xt2 : Float =  gears(1).holder.xt
      val hta2 : Float = gears(1).holder.hta
      val alpha_t2 : Float  = gears(1).holder.alpha_t
      val alpha_tw2 : Float = structure.getGearConnectionList(0).connectionCalculationBehavior.alpha_tw
      val y2 = z(1) * (math.cos(alpha_t2) / math.cos(alpha_tw2) - 1);
      val dy2 = 2 * xt2 - y2;

      val mt3 : Float = gears(2).holder.mt		// 2й ряд
      val xt3 : Float = gears(2).holder.xt
      val hta3 : Float = gears(2).holder.hta
      val alpha_t3 : Float = gears(2).holder.alpha_t
      val alpha_tw3 : Float = structure.getGearConnectionList(1).connectionCalculationBehavior.alpha_tw
      val y3 = z(2) * (math.cos(alpha_t3) / math.cos(alpha_tw3) - 1)
      val dy3 = 2 * xt3 - y3;
      val B2 = (z(1) / 2 + xt2 + hta2 - dy2);
      val B3 = (z(2) / 2 + xt3 + hta3 - dy3);
      if (B2 >= B3)
      {
        val ans = (A > B2);
        ans
      }
      else
      {
        val ans = (A > B3);
        ans
      }
    }*/
    //first, we must get the biggest gear on our satellite
    val max_wheel : GearWheel = structure.getSatelliteGears.maxBy(_.holder.da)
    //now we must get the connection where our max_wheel is located
    val connection : GearConnection = structure.getGearConnectionList.find(connection =>
      if (connection.gear1 == max_wheel || connection.gear2 == max_wheel) true else false).get
    //after we got our connection we must calculate the distance between the axes of the satellites
    val distance_between_centers = connection.connectionCalculationBehavior.aw * 2.0f * math.sin(math.Pi / structure.storage.k.toFloat).toFloat
    val difference = distance_between_centers - max_wheel.holder.da
    if (difference >= max_wheel.holder.m * 0.5f){
      true
    } else {
      false
    }
  }

  override def U14_h: Double = getGearRatioCarrierStopped
  override def fullConditionCheck : FullConditionCheck = {
    //we go on all condition
    val gear_ratio = getGearRatio
    val alignment = alignmentCondition
    val assembly = assemblyCondition
    val intereference = interferenceAll
    val neighborhood = neighborhoodCondition
    val noPruning = noPruningOnAll
    FullConditionCheck(gear_ratio.toFloat, alignment,assembly, intereference, neighborhood, noPruning)
  }

  override def interferenceAll: Boolean = {
    structure.getGearConnectionList.map(_.connectionCalculationBehavior.interference).foldLeft(true)(_ && _)
  }
}