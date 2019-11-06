package planar_interface.view.OptionsView

import javafx.scene.Parent
import planar_interface.model.{CurrentMode, KINEMATIC_ANALYSIS_FORWARD, KINEMATIC_SYNTHESIS, MechanismDatabase, STRENGTH_ANALYSIS_FORWARD, STRENGTH_SYNTHESIS}
import planar_interface.view.GearGroupListView.{AbstractGearGroupListViewControllerFactory, GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_structure.mechanism.{GearGroup, GearWheel, Mechanism}

import scala.collection.mutable
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
case class ControllerStatusReport(mechanismChanged : Boolean, modeChanged : Boolean)

class MechanismControllerConcrete extends MechanismController with Observable {
  /**
   * optionsViewController saves current view of Options and gives access to its elements
   */
  protected var optionsViewController : OptionsViewController = _
  /**
   * can create new OptionViewControllers
   */
  protected var optionsViewControllerFactory : AbstractOptionsViewControllerFactory = _
  /**
   * stores current mechanism
   */
    init()
  def init() : Unit = {
    optionsViewControllerFactory = new OptionsViewControllerFactory
    optionsViewController = optionsViewControllerFactory.createView().asInstanceOf[OptionsViewController]
  }
  override protected val mechanismDatabase: MechanismDatabase = new MechanismDatabase()
  //tracks current status
  var currentStatus : ControllerStatusReport = ControllerStatusReport(mechanismChanged = true, modeChanged = true)
  override def setMode(s: String): Unit = {
    s match {
      case "KINEMATIC_ANALYSIS_FORWARD" => mechanismDatabase.currentMode= KINEMATIC_ANALYSIS_FORWARD
      case "KINEMATIC_SYNTHESIS" => mechanismDatabase.currentMode= KINEMATIC_SYNTHESIS
      case "STRENGTH_ANALYSIS_FORWARD" => mechanismDatabase.currentMode= STRENGTH_ANALYSIS_FORWARD
      case "STRENGTH_SYNTHESIS" => mechanismDatabase.currentMode= STRENGTH_SYNTHESIS
      case _ => println("Mode can't be set")
    }
    currentStatus = ControllerStatusReport(mechanismChanged = false, modeChanged = true)
    notifyObservers()
  }

  override def getMode: String = mechanismDatabase.currentMode.toString
  override def updateGearViewWith(parent : Parent): Unit = {
    optionsViewController.optionsView.appBorderPane.setCenter(parent)
  }
  override def setMechanism(s: String): Unit = {
    if (mechanismDatabase.makeMechanism(s)){
      println("Successfully made mechanism")
      currentStatus = ControllerStatusReport(mechanismChanged = true, modeChanged = false)
      notifyObservers()
    } else {
      println("Failure when making mechanism")
    }
  }

  override def getMechanism: Mechanism = mechanismDatabase.mechanism
  override def getStatus: AnyRef = {
    currentStatus
  }
  override def setMode(s: CurrentMode): Unit = {
    setMode(s.toString)
  }

  override def getParent: Parent = optionsViewController.optionsView.appBorderPane

  override def notifyObservers(): Unit = {
    super.notifyObservers()
    currentStatus = ControllerStatusReport(mechanismChanged = false, modeChanged = false)
  }
}




trait MechanismController{
  protected val mechanismDatabase : MechanismDatabase
  def setMode(s : String) : Unit
  def setMode(s : CurrentMode) : Unit
  def getMode : String
  //updates all views to current mode, etc
  def updateGearViewWith(parent : Parent) : Unit
  //set current mechanism
  def setMechanism(s : String) : Unit
  def getMechanism : Mechanism
  def getParent : Parent
}

/**
 * Changes current GearGroupView mode if notified that the menu changed
 * Takes the new mechanism in case it was changed and then adapts the corresponding
 * visual elements to the new mechanism
 * Then attaches new visual elements to the higher view class
 */
class GearViewMenu extends  Observer {
  protected var currentGearViewController : GearGroupListViewController = _
  protected var currentGearViewControllerFactory : AbstractGearGroupListViewControllerFactory = _
  override protected var observable: Observable = _
  init()
  //TODO list with factories for controllers for every mode
  protected def observableController : MechanismController = observable.asInstanceOf[MechanismController]
  /**
   * @note for storing old view and reusability of old created views in order to not create new view for old modes and mechanisms
   */
  def init() : Unit = {
    currentGearViewControllerFactory = new GearGroupListViewControllerFactory
  }
  def updateView(): Unit = {
    observableController.updateGearViewWith(
      savedModes(observableController.getMechanism)(observableController.getMode).gearGroupListView.gearGroupList)
  }
  protected val savedModes : mutable.HashMap[Mechanism, mutable.HashMap[String, GearGroupListViewController]] =
    mutable.HashMap.empty[Mechanism, mutable.HashMap[String, GearGroupListViewController]]
  override def onChange(): Unit = {
    if (observable != null){
      val status = observable.getStatus.asInstanceOf[ControllerStatusReport]
      //if at least something has changed we must understand what to do
      if (status.mechanismChanged || status.modeChanged){
        val mechanism = observableController.getMechanism
        savedModes get mechanism match {
            //if we found an old mechanism then we must check if there is the necessary view for it
          case Some(value) => value get observableController.getMode match {
            case Some(view) => updateView()
            //in case we don't have such view yet we must create a new one
            case None => {
              currentGearViewControllerFactory.setGearGroups(mechanism.characteristics.gearStructureCharacteristic.getGearGroups)
              //creating new view based on the mechanism of the observableController
              val new_view = currentGearViewControllerFactory
                .createView(observableController.getMechanism.characteristics.gearStructureCharacteristic.getGearGroups)
                .asInstanceOf[GearGroupListViewController]
              //now we must store the view, we are in this branch of case conditions thus the mechanism is already exist in our DB
              value.addOne(observableController.getMode -> new_view)
              updateView()
            }
          }
          //we didn't find such mechanism before
          case None => {
            val mechanism = observableController.getMechanism
            //creating new view based on the mechanism of the observableController
            val new_view = currentGearViewControllerFactory
              .createView(mechanism.characteristics.gearStructureCharacteristic.getGearGroups)
              .asInstanceOf[GearGroupListViewController]
            //adding an entrance for the new mechanism
            savedModes.addOne(mechanism -> mutable.HashMap.empty[String, GearGroupListViewController])
            savedModes(mechanism).addOne(observableController.getMode -> new_view)
            updateView()
          }
        }
      }
    }
  }
}

abstract  class AbstractMechanismControllerFactory{
  def createController() : MechanismController
}

class MechanismControllerFactory extends  AbstractMechanismControllerFactory {
  override def createController(): MechanismController = {
    val mechanismController = new MechanismControllerConcrete
    val gearViewMenu = new GearViewMenu
    //setting up proper observer
    mechanismController.addObserver(gearViewMenu)
    mechanismController.notifyObservers()
    mechanismController
  }
}