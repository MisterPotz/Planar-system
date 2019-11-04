class Main{
  /*def main(args: Array[String]): Unit = { //Schedule a job for the event dispatch thread:
    //creating and showing this application's GUI.
  /*  javax.swing.SwingUtilities.invokeLater(new Runnable() {
      override def run(): Unit = {createAndShowGUI()}})*/
  /*  val mech = Mechanism2KH("InternalInternal_CarrierInput")
    val gears = mech.characteristics.gearStructureCharacteristic.getGearList
    gears(0).holder.z = 87
    gears(1).holder.z = 34
    gears(2).holder.z = 28
    gears(3).holder.z = 81
    println(s"${mech.methods.geometricMethods.getGearRatio}") //TODO DANGEROUS BUG with all gears being interconnected with prepareAllGears functions. Must be overloaded in classes
    mech.characteristics.gearStructureCharacteristic.getGearGroup(0)
    println(s"${mech.methods.geometricMethods.noPruningOnAll}")
    println(s"${mech.methods.geometricMethods.neighborhoodCondition}")
    println(s"${mech.methods.geometricMethods.assemblyCondition}")
    println(s"${mech.methods.geometricMethods.alignmentCondition}")
    println(s"${mech.methods.geometricMethods.interferenceCondition(0)}")
    println(s"${mech.methods.geometricMethods.interferenceCondition(1)}")*/
    val mech = Mechanism2KH("InternalInternal_CarrierInput")
    val gear = mech.characteristics.gearStructureCharacteristic.getGear(0)
    launch(args)
   // EventQueue.invokeLater( () => {var ex = new SimpleEx(); ex.setVisible(true) })
  }*/
}

object Main{
  def smain(args: Array[String]): Unit = {
    println("Works")
  }
}
