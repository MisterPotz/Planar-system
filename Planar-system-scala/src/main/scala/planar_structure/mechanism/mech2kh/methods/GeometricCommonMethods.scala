package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.{Characteristic, GearStructureCharacteristic, GearWheel, GeometricMethod}

//these methods are common for every type of mechanism
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
  override def interferenceCondition(i : Int): Boolean = {
    methodable.getGearConnectionList(i).connectionCalculationBehavior.interference
  }
  override def minimalSizeComparingTo(a: List[Characteristic]): Boolean = {
    if (minimalSize(a).asInstanceOf[GearStructureCharacteristic].getMaxRw >= methodable.getMaxRw){
      true
    } else false
  }

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