package planar_structure.help_traits

import scala.collection.mutable.ArrayBuffer

trait Storage[A] extends ArrayBuffer[A] with FuncContainer[A]{
  protected val collection : ArrayBuffer[A] = new ArrayBuffer[A]()
  override def apply(a : Int) : A = collection(a)

  override def addAll(elems: IterableOnce[A]): Storage.this.type = {
    collection.addAll(elems); this
  }
  override def length: Int = collection.length
  override def addOne(elem: A): Storage.this.type ={collection.addOne(elem); this}
  override def filter(pred: A => Boolean): ArrayBuffer[A] = {collection.filter(pred); this}
  override def toString : String = {
    collection.toString
  }
  override def remove(index: Int): A = collection.remove(index)

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty
}