package planar_structure.mechanism.report.file

//contains desciption of report
sealed trait Report{
  def name : String
  def descriptionFull : String
  def descriptionShort : String
  val modifier : String = "\n\t"
}

class ListedReport(val title : String, val list : List[Report])
  extends Report{
  override def name : String = title
  override def descriptionFull: String ={
    list.foldLeft(s"${name}")(_ +"\n"+ _.descriptionFull)
      .split("\n").mkString(modifier)
  }
  override def descriptionShort: String = {
    list.foldLeft(s"${name}")(_ +"\n"+ _.descriptionShort)
      .split("\n").mkString(modifier)
  }
}

abstract class ElemReport[T](val title : String, val status : T) extends Report{
  override def name: String = title
}

case class BoolElemReport(override val title : String, override val status : Boolean)
  extends ElemReport[Boolean](title, status) {
  override def descriptionFull: String = s"$name: $status"
  override def descriptionShort: String = s"$name: $status"
}

case class IntElemReport(override val title : String, override val status : Int)
  extends ElemReport[Int](title, status) {
  override def descriptionFull: String = s"$name: $status"
  override def descriptionShort: String = s"$name: $status"
}