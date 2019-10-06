package planar_structure.help_traits

import planar_structure.core_structure.connections.GearConnection
import planar_structure.core_structure.{BaseGearWheel, BaseLink}

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
  def toStringShort : String
}
trait FilterableSingle[A,U]{
  def filter(pred : A => Boolean) : U
}
trait FilterableTuple[A,B, U]{
  def filter(pred : ((A, B)) => Boolean) : U
}
//storages are intended for baselinks and its subclasses
trait Storage[A <:Storable] extends BasicStorage[ArrayBuffer[A],A, Int] with FuncContainer[A] with FilterableSingle[A,ArrayBuffer[A]] with BeautifulDebugOutput  {
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
  //short format for pleasing console output
  override def toStringShort : String = {
    collection.foldLeft("")((left : String, right : A) => left + "\n" + right.toStringShort)
  }
  override def remove(index: Int): A = collection.remove(index)

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty
}
trait StorageHash[A <: Storable] extends BasicStorage[mutable.HashMap[Int, A],(Int, A), Int] with FilterableTuple[Int, A, mutable.HashMap[Int, A]]{
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
  //short format for pleasing console output
  override def toStringShort : String = {
    collection.foldLeft("")((left : String, right : (Int, A)) => left + "\n" + right._2.toStringShort)
  }
  override def remove(index: Int): (Int, A) = {
    val temp = collection(index);
    collection.remove(index);
    (index, temp);
  }

  override def isEmpty: Boolean = collection.isEmpty

  override def nonEmpty: Boolean = collection.nonEmpty
}
trait StringOrderModifier [U]{
  def stringCreator (sortedSpecially : () => Array[U]): String
}
trait BaseStorageHashD[A, U <: Storable] extends BasicStorage[mutable.HashMap[A, U],(A, U), A] with FilterableTuple[A, U, mutable.HashMap[A, U]] {
  val collection : mutable.HashMap[A, U] = new mutable.HashMap[A, U]()
  def apply(a : A) : (A,U) = (a,  collection(a))
  override def addAll(elems: (A, U)*): BaseStorageHashD[A,U] = {
    for (elem <- elems){
      collection.addOne(elem)
    }
    this
  }

  override def length: Int = collection.knownSize
  override def addOne(elem: (A, U)): BaseStorageHashD[A,U]  ={collection.addOne(elem); this}
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

  override def toStringShort: String ={
    collection.foldLeft("")((left : String, right : (A, U)) => left + "\n" + right._2.toStringShort)
  }
}
trait StorageHashD[A, U <: Storable] extends BaseStorageHashD[A,U] with StringOrderModifier[U]

abstract class StorageHashDConnection extends StorageHashD[(BaseGearWheel,BaseGearWheel), GearConnection[BaseGearWheel,BaseGearWheel]]{
  //this one initialized manually
  override def stringCreator(sortedSpecially : () => Array[GearConnection[BaseGearWheel, BaseGearWheel]]): String = {
    sortedSpecially().foldLeft("")((left : String, right : GearConnection[BaseGearWheel,BaseGearWheel])=> left + "\n" + right.toStringShort)
  }
  override def toStringShort : String = {
    super.toStringShort
  }
  def toStringShortSmart(sortedSpecially : () => Array[GearConnection[BaseGearWheel, BaseGearWheel]]) : String = {
    stringCreator(sortedSpecially)
  }
}
