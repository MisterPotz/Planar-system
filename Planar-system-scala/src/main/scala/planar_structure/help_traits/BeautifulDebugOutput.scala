package planar_structure.help_traits

trait BeautifulDebugOutput {
  def print(s : String): String = {
    val string = s.split("\n").map(_ + "\n\t").foldLeft("\t")(_ + _) //+ "\n"
    string.slice(0, string.lastIndexOf("\n"))
  }
}
