package planar_interface

import javafx.scene.Parent
import planar_interface.model.{CurrentMode, KINEMATIC_ANALYSIS_FORWARD, KINEMATIC_SYNTHESIS, MechanismDatabase, STRENGTH_ANALYSIS_FORWARD, STRENGTH_SYNTHESIS}
import planar_interface.view.OptionsView.{AbstractOptionsViewControllerFactory, OptionsViewController, OptionsViewControllerFactory}
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

import scala.collection.mutable.ArrayBuffer

case class ControllerStatusReport(mechanismChanged : Boolean, modeChanged : Boolean)

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
  def makeCode(mechType : MechanismType, carrierType : CarrierPosition) : String = {
    mechType.toCode + "_" + carrierType.toCode
  }
  def updateMechanism() : Unit = {
    val mech_type_view = optionsViewController.optionsView.mechanismTypeCombo
    val carrier_type_view = optionsViewController.optionsView.carrierPosCombo
    setMechanism(makeCode(mech_type_view.getSelectionModel.getSelectedItem, carrier_type_view.getSelectionModel.getSelectedItem))
  }
  def init() : Unit = {
    optionsViewControllerFactory = new OptionsViewControllerFactory
    optionsViewController = optionsViewControllerFactory.createView().asInstanceOf[OptionsViewController]
    optionsViewController.optionsView.mechanismTypeCombo.setOnAction(event => {
      updateMechanism()
      //TODO check functionality
    })
    optionsViewController.optionsView.carrierPosCombo.setOnAction(event => {
      updateMechanism()
    })
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
      println("Successfully set mechanism")
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




