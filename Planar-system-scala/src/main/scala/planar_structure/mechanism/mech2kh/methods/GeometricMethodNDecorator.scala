package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.{GearWheel, GeometricMethod}

/**
 *
 * @param dec decorated initial method
 */
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