package planar_interface

import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.Parent
import javafx.scene.layout.BorderPane
import planar_interface.model.MechanismDatabase
import planar_interface.view.KinematicSynthesisInputView.KinematicSynthesisArgsPartial
import planar_interface.view.OptionsView.{AbstractOptionsViewControllerFactory, OptionsViewController, OptionsViewControllerFactory}
import planar_interface.view.event_types.{ArgumentGetterEvent, BundleKeeper, CalculatingResultObtained, CalculationStarted, EnterEvent, EraseEvent, MechanismChangedEvent, ModeChangedEvent, SuccessfulEnter, UnsuccessfulEnter}
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.types._

import scala.collection.mutable.ArrayBuffer
trait MechanismController{
  protected val mechanismDatabase : MechanismDatabase
 // def setMode(s : String) : Unit
 // def setMode(s : ProcessType) : Unit
  def getMode : String
  //updates all views to current mode, etc
  def updateGearViewWith(parent : Parent) : Unit
  //set current mechanism
  def setMechanism(s : String) : Unit
  def getMechanism : Mechanism
  def getMechanismCode : String
  def getMechanismType : MechanismType
  def getParent : Parent
  def getBorder : BorderPane
  def successfulEnter(success : Boolean = false) : Unit
}



class MechanismControllerConcrete extends MechanismController with Observable with Observer {
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
    addObserver(optionsViewController)
    optionsViewController.optionsView.mechanismTypeCombo.setOnAction(event => {
      updateMechanism()
      notifyObservers(MechanismChangedEvent(optionsViewController.optionsView.mechanismTypeCombo.getValue))
      //TODO check functionality
    })
    optionsViewController.optionsView.carrierPosCombo.setOnAction(event => {
      updateMechanism()
      notifyObservers(MechanismChangedEvent(optionsViewController.optionsView.mechanismTypeCombo.getValue))
    })
    optionsViewController.optionsView.modeTypeCombo.setOnAction(onModeChange)
    optionsViewController.optionsView.enterButton.setOnAction(onEnter)
    optionsViewController.optionsView.calculateButton.setOnAction(onCalculate)
    optionsViewController.optionsView.eraseButton.setOnAction(onErase)
  }
  def onErase : EventHandler[ActionEvent] = event => {
    notifyObservers(EraseEvent)
  }
  def onModeChange : EventHandler[ActionEvent] = event => {
    val new_mode = optionsViewController.optionsView.modeTypeCombo.getValue
    mechanismDatabase.currentMode= new_mode.toCode
    notifyObservers(ModeChangedEvent(new_mode))
  }
  def onCalculate : EventHandler[ActionEvent] = event => {
    val bundle_keeper : BundleKeeper = new BundleKeeper
    notifyObservers(ArgumentGetterEvent(bundle_keeper))
    notifyObservers(CalculationStarted)
    //here supposed that we obtained some useful info from bundle_keeper =>
    calculateWithBundle(bundle_keeper)
  }
  def calculateWithBundle(bundleKeeper: BundleKeeper) : Unit = {
    bundleKeeper.bundle match {
      case KinematicSynthesisArgsPartial(z_min_max, k, u1h, eps_u1h,additionalWheelParams) =>
        val full_arg : KinematicSynthesisArgs =
          KinematicSynthesisArgs(z_min_max, k, u1h, eps_u1h,
            mechanismDatabase.mechanism.getMechanismType,
            mechanismDatabase.mechanism.getCarrierPosition, additionalWheelParams)
        mechanismDatabase.processCurrentMode(full_arg)
      case _ => mechanismDatabase.processCurrentMode()
    }
  }
  def onEnter : EventHandler[ActionEvent] = event => {
    notifyObservers(EnterEvent) //TODO fire an enter event
  }
  override protected val mechanismDatabase: MechanismDatabase = new MechanismDatabase()
  mechanismDatabase.addObserver(this)
  //tracks current status
 /* override def setMode(s: String): Unit = {
    s match {
      case "KINEMATIC_ANALYSIS_FORWARD" => mechanismDatabase.currentMode= KINEMATIC_ANALYSIS_FORWARD
      case "KINEMATIC_SYNTHESIS" => mechanismDatabase.currentMode= KINEMATIC_SYNTHESIS
      case "STRENGTH_ANALYSIS_FORWARD" => mechanismDatabase.currentMode= STRENGTH_ANALYSIS_FORWARD
      case "STRENGTH_SYNTHESIS" => mechanismDatabase.currentMode= STRENGTH_SYNTHESIS
      case _ => println("Mode can't be set")
    }
    currentStatus = ControllerStatusReport(mechanismChanged = false, modeChanged = true)
    notifyObservers()
  }*/
  override def getMode: String = mechanismDatabase.currentMode
  override def updateGearViewWith(parent : Parent): Unit = {
    optionsViewController.optionsView.appBorderPane.setCenter(parent)
  }
  override def setMechanism(s: String): Unit = {
    if (mechanismDatabase.makeMechanism(s)){
      println("Successfully set mechanism")
      notifyObservers(MechanismChangedEvent(getMechanism.getMechanismType))
    } else {
      println("Failure when making mechanism")
    }
  }
  override def getMechanism: Mechanism = mechanismDatabase.mechanism
  /*override def setMode(s: ProcessType): Unit = {
    setMode(s.toString)
  }*/

  override def getParent: Parent = optionsViewController.optionsView.appBorderPane

  protected def lockElementsToWait(lock : Boolean) = { //locks corresponding elements when the calculation begins
    optionsViewController.setLock(lock)
  }
  override def notifyObservers(event: Event): Unit = {
    super.notifyObservers(event)}

  override def getBorder: BorderPane = optionsViewController.optionsView.appBorderPane

  override protected var observable: Observable = mechanismDatabase

  override def onChange(event: Event): Unit = {
    event match {
      case a : CalculatingResultObtained =>
        lockElementsToWait(false)
        notifyObservers(a)
      case some : Event => notifyObservers(some)
    }
  }
  override def successfulEnter(success : Boolean = false) : Unit = {
    if (success) {
      notifyObservers(SuccessfulEnter)
    }
    else
      notifyObservers(UnsuccessfulEnter)
  }

  override def getMechanismCode: String = getMechanism.gearStructureCharacteristic.storage.mechanismType.toCode

  override def getMechanismType: MechanismType = getMechanism.gearStructureCharacteristic.storage.mechanismType
}




