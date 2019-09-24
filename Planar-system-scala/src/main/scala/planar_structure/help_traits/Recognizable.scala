package planar_structure.help_traits

//trait for implicit conversions from general to particular
trait Recognizable[T]{
  //non-safe version
  implicit def super2SubClass[U <: T](t : T) : U
  //safe version
  implicit def sub2Option(t : T) : Option[T]
}