package planar_structure.help_traits

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait BasicStorage[T, U] {self=>
  val collection : T
  def apply(a : Int) : U
  def addAll(elems: IterableOnce[U]): self.type
  def length: Int
  def addOne(elem: U): self.type
  def filter(pred: U => Boolean): self.type
  def toString : String
  def remove(index: Int): U
  def isEmpty: Boolean
  def nonEmpty: Boolean
}

trait Storage[A] extends BasicStorage[ArrayBuffer[A],A] with FuncContainer[A]{
  val collection : ArrayBuffer[A] = new ArrayBuffer[A]()
  override def apply(a : Int) : A = collection(a)
  override def addAll(elems: IterableOnce[A]): Storage.this.type = {
    collection.addAll(elems)
    this
  }
  override def length: Int = collection.length
  override def addOne(elem: A): Storage.this.type ={collection.addOne(elem); this}
  override def filter(pred: A => Boolean): Storage.this.type = {collection.filter(pred); this}
  override def toString : String = {
    collection.toString
  }
  override def remove(index: Int): A = collection.remove(index)

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty
}