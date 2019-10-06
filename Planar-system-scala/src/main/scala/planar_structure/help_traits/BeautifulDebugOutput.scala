package planar_structure.help_traits

trait BeautifulDebugOutput {
  def toStringShort: String = super.toString
  def toStringFull : String = super.toString
}

//TODO print function now just prints, the in-built function, be careful
object BeautifulDebugOutput{
  def print(s : String): String = {
    val string = s.split("\n").map(_ + "\n\t").foldLeft("\t")(_ + _) //+ "\n"
    string.slice(0, string.lastIndexOf("\n"))
  }
  def subFirst(s : String) : String = {
    val list = s. split("\n").map(_ + "\n\t").foldLeft("")(_+_)
    list.slice(0, list.lastIndexOf("\n"))
  }
}