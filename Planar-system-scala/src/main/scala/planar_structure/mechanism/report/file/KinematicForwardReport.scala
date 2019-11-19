package planar_structure.mechanism.report.file

import planar_structure.mechanism.Mechanism

case class KinematicForwardReport(mechanism: Mechanism, override val list : List[Report])
  extends ListedReport("Условия кинематического синтеза", list){
  override def descriptionFull: String = {
    mechanism.toStringFull + "\n" +  super.descriptionFull
  }

  override def descriptionShort: String = {
    mechanism.toStringShort + "\n" +
      super.descriptionShort}
}



