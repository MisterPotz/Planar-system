package planar_structure.help_traits

trait Updateable[+A]{self=>
  def update() : self.type
}
