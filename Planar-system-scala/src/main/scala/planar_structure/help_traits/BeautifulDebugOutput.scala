package planar_structure.help_traits

trait BeautifulDebugOutput {
  def print(s : String): String =  s.split("\n").map(_ + "\n\t").foldLeft("")(_+_) + "\n"
}
