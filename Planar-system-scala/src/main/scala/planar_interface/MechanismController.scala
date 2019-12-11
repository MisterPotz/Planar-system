package planar_interface

import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.Parent
import javafx.scene.control.{Button, ComboBox}
import javafx.scene.layout.BorderPane
import planar_interface.model.MechanismDatabase
import planar_interface.view.GearViewMenu
import planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView.KinematicSynthesisArgsPartial
import planar_interface.view.OptionsView.{AbstractOptionsViewControllerFactory, OptionsViewController, OptionsViewControllerFactory}
import planar_interface.view.event_types.{Calculate, CalculatingResultObtained, CalculationStarted, EnterEvent, EraseEvent, InitialEvent, MechanismChangedEvent, MechanismConfigChanged, ModeChangedEvent, SuccessfulEnter, UnsuccessfulEnter}
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.types._



trait MechanismController{
  //updates all views to current mode, etc
  def updateGearViewWith(parent : Parent) : Unit
  def getParent : Parent
  def getBorder : BorderPane
  def successfulEnter(success : Boolean = false) : Unit
}



object MechanismControllerConcrete extends MechanismController with Observer with Observable {
  override protected var observable: Observable = _

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
    val setup = (modeTypeCombo : ComboBox[ProcessType],
              enterButton : Button,
              calculateButton : Button,
              eraseButton : Button)  => {
      modeTypeCombo.setOnAction(onModeChange)
      enterButton.setOnAction(onEnter)
      calculateButton.setOnAction(onCalculate)
      eraseButton.setOnAction(onErase)
    }
    optionsViewControllerFactory = new OptionsViewControllerFactory
    optionsViewController = optionsViewControllerFactory.createView().asInstanceOf[OptionsViewController]
    addObserver(optionsViewController)
    optionsViewController.setup(setup)
  }
  def onErase : EventHandler[ActionEvent] = event => {
    notifyObservers(EraseEvent)
  }
  def onModeChange : EventHandler[ActionEvent] = event => {
    notifyObservers(ModeChangedEvent(optionsViewController.getMode()))
  }
  def onCalculate : EventHandler[ActionEvent] = event => {
    notifyObservers(Calculate)
  }
  def onEnter : EventHandler[ActionEvent] = event => {
    notifyObservers(EnterEvent) //TODO fire an enter event
  }

  //override def getMode: String = optionsViewController.optionsView.modeTypeCombo.getSelectionModel.getSelectedItem.toCode
  override def updateGearViewWith(parent : Parent): Unit = {
    optionsViewController.setInputView(parent)
  }

  override def getParent: Parent = optionsViewController.optionsView.appBorderPane

  override def notifyObservers(event: Event): Unit = {
    super.notifyObservers(event)}

  override def onChange(event: Event): Unit = {
    event match {
      case a : CalculatingResultObtained =>
        notifyObservers(a)
      case a : MechanismConfigChanged => {
        notifyObservers(a)
      }
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
  override def getBorder: BorderPane = ???

}


object PlanarSystemLauncher{
  def createController(): MechanismController = {
    val mechanismController =  MechanismControllerConcrete
    val gearViewMenu = new GearViewMenu
    // val resultsViewMenu = new ResultsViewMenu
    //setting up proper observer
    mechanismController.addObserver(gearViewMenu)
    // mechanismController.addObserver(resultsViewMenu)
    mechanismController.notifyObservers(InitialEvent())
    mechanismController
  }
}



