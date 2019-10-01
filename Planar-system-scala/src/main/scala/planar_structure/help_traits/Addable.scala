package planar_structure.help_traits

trait Addable[U] {self=>

  def add[A <: U](a: A): Addable[U]

  def addAll[A <: U](a: A*):Addable[U]

}
