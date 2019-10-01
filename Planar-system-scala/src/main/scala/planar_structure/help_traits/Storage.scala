package planar_structure.help_traits

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait BasicStorage[T,U] {self=>
  val collection : T
  def apply(a : Int) : U
  def addAll(elems: U*): self.type
  def length: Int
  def addOne(elem: U): self.type
  def toString : String
  def remove(index: Int): U
  def isEmpty: Boolean
  def nonEmpty: Boolean
}
trait FilterableSingle[A,U]{
  def filter(pred : A => Boolean) : U
}
trait FilterableTuple[A,B, U]{
  def filter(pred : ((A, B)) => Boolean) : U
}
trait Storage[A] extends BasicStorage[ArrayBuffer[A],A] with FuncContainer[A] with FilterableSingle[A,ArrayBuffer[A]] {
  val collection : ArrayBuffer[A] = new ArrayBuffer[A]()
  override def apply(a : Int) : A = collection(a)
  override def addAll(elems: A*): Storage.this.type = {
    collection.addAll(elems)
    this
  }
  override def length: Int = collection.length
  override def addOne(elem: A): Storage.this.type ={collection.addOne(elem); this}
  override def filter(pred: A=> Boolean): ArrayBuffer[A] = {
    collection.filter(pred)
  }
  override def toString : String = {
    collection.toString
  }
  override def remove(index: Int): A = collection.remove(index)

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty
}
trait StorageHash[A] extends BasicStorage[mutable.HashMap[Int, A],(Int, A)] with FuncContainer[A] with FilterableTuple[Int, A, mutable.HashMap[Int, A]]{
  val collection : mutable.HashMap[Int, A] = new mutable.HashMap[Int, A]()
  override def apply(a : Int) : (Int, A) = (a, collection(a))
  override def addAll(elems: (Int, A)*): StorageHash.this.type = {
    for (elem <- elems){
      collection.addOne(elem)
    }
    this
  }
  override def length: Int = collection.knownSize
  override def addOne(elem: (Int, A)): StorageHash.this.type ={collection.addOne(elem); this}
  override def filter(pred: ((Int, A)) => Boolean): mutable.HashMap[Int,A] = {collection.filter(pred)}
  override def toString : String = {
    collection.toString
  }
  override def remove(index: Int): (Int, A) = {
    val temp = collection(index);
    collection.remove(index);
    (index, temp);
  }

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty
}