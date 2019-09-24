package planar_structure.help_traits

trait Addable[U] {self=>

  def add[A <: U](a: A): self.type

  def addAll[A <: U](a: A*): self.type
}
