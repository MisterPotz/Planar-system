package planar_structure.help_traits

import planar_structure.core_structure.BaseGearWheel

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait BasicStorage[T,U, Z] {self=>
  val collection : T
  def apply(a : Z) : U
  def addAll(elems: U*): BasicStorage[T, U,Z]
  def length: Int
  def addOne(elem: U): BasicStorage[T,U,Z]
  def toString : String
  def remove(index: Z): U
  def isEmpty: Boolean
  def nonEmpty: Boolean
}
trait FilterableSingle[A,U]{
  def filter(pred : A => Boolean) : U
}
trait FilterableTuple[A,B, U]{
  def filter(pred : ((A, B)) => Boolean) : U
}
trait Storage[A] extends BasicStorage[ArrayBuffer[A],A, Int] with FuncContainer[A] with FilterableSingle[A,ArrayBuffer[A]] {
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
trait StorageHash[A] extends BasicStorage[mutable.HashMap[Int, A],(Int, A), Int] with FilterableTuple[Int, A, mutable.HashMap[Int, A]]{
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
class StorageHashD[A, U] extends BasicStorage[mutable.HashMap[A, U],(A, U), A] with FilterableTuple[A, U, mutable.HashMap[A, U]]{
  val collection : mutable.HashMap[A, U] = new mutable.HashMap[A, U]()
  def apply(a : A) : (A,U) = (a,  collection(a))
  override def addAll(elems: (A, U)*): StorageHashD[A,U] = {
    for (elem <- elems){
      collection(elem._1) = elem._2
    }
    this
  }
  override def length: Int = collection.knownSize
  override def addOne(elem: (A, U)): StorageHashD[A,U]  ={collection(elem._1) = elem._2; this}
  def filter(pred: ((A, U)) => Boolean): mutable.HashMap[A,U] = {collection.filter(pred)}
  override def toString : String = {
    collection.toString
  }
  override def remove(index: A): (A,U) = {
    val temp = collection(index);
    collection.remove(index);
    (index, temp)
  }

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty


}