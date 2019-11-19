import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.report.file.{BoolElemReport, IntElemReport, ListedReport, Report}



object Main extends App {
  val report : Report = new ListedReport("listed report",
    BoolElemReport("Условие соосности", status = true) ::
      IntElemReport("Число зубьев", 45) ::
      new ListedReport("Подпроверки",BoolElemReport("подрезание", true) :: BoolElemReport("интерференция", true) :: Nil) ::
      Nil)
  println(s"${report.descriptionShort}")
  val mech = Mechanism2KH.apply("ExternalInternal_CarrierOutput")
  val gears = mech.getGears
  gears(0).holder.z = 40
  gears(1).holder.z = 40
  gears(2).holder.z = 20
  gears(3).holder.z = 100
  mech.getStorage.k = 3
  println(s"Gear ratio: ${mech.methods.getGearRatio}\nAlignment: ${mech.methods.alignmentCondition}")
  println(s"Neighborhood: ${mech.methods.neighborhoodCondition}\nAssembly: ${mech.methods.assemblyCondition}")
  println(s"Pruning: ${mech.methods.noPruningOnAll}\nInterference: ${mech.methods.interferenceCondition(1)}")
  println(s"Satellites amount: ${mech.getStorage.k}")

}
