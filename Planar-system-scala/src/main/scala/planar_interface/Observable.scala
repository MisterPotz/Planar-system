package planar_interface

import scala.collection.mutable.ArrayBuffer

trait Observable{
  // array of listeners
  val observers : ArrayBuffer[Observer] = ArrayBuffer.empty[Observer]
  def addObserver(observer : Observer): Unit = {
    observer.setObservable(this)
    observers.addOne(observer)
  }
  def removeObserver(observer: Observer): Observer = {
    observer.removeObservable()
    //remove the observer
    observers.remove(observers.indexWhere(_ eq observer))
  }
  def notifyObservers() : Unit = {
    observers.foreach(_.onChange())
  }
  def getStatus : AnyRef //is defined in subclasses, gives info about what has changed
}
trait Observer{
  protected var observable : Observable
  def removeObservable() : Unit = {
    observable = null
  }
  def setObservable(observable: Observable) : Unit = {
    this.observable = observable
  }
  def onChange() : Unit
}