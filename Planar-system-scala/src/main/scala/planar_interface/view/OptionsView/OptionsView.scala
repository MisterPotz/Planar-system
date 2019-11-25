package planar_interface.view.OptionsView

import java.net.URL

import planar_structure.mechanism.types._
import javafx.collections.{FXCollections, ObservableList}
import javafx.fxml.FXML
import javafx.scene.control.{Button, ComboBox, MenuBar}
import javafx.scene.layout.{AnchorPane, BorderPane}
import planar_interface.{Event, Observable, Observer}
import planar_interface.view.GearGroupView.AbstractGearGroupOnlyViewControllerFactory
import planar_interface.view.GearView.ViewFactory
import planar_interface.view.event_types.{CalculatingResultObtained, CalculationStarted, InitialEvent, MechanismChangedEvent, ModeChangedEvent, SuccessfulEnter, UnsuccessfulEnter}

class OptionsView {
  @FXML
  var carrierPosCombo : ComboBox[CarrierPosition] = _
  @FXML
  var modeTypeCombo : ComboBox[ProcessType] = _
  @FXML
  var mechanismTypeCombo : ComboBox[MechanismType] = _
  @FXML
  var appBorderPane : BorderPane = _
  @FXML
  var eraseButton : Button = _
  @FXML
  var enterButton : Button = _
  @FXML
  var settingsAnchorPane : AnchorPane = _
  @FXML
  var resultsAnchorPane : AnchorPane = _
  @FXML
  var appMenuBar : MenuBar =  _
  @FXML
  var calculateButton : Button = _
  @FXML
  var cancelButton : Button = _
}
class OptionsViewController(var optionsView: OptionsView) extends Observer{
  init()
  def init() : Unit = {
    setupMode();
    setupMechanism()
    setupCarrier()
    setLock(false)
  }
  def setLock(bool : Boolean) = {
    optionsView.carrierPosCombo.setDisable(bool)
    optionsView.modeTypeCombo.setDisable(bool)
    optionsView.mechanismTypeCombo.setDisable(bool)
    optionsView.eraseButton.setDisable(bool)
    optionsView.enterButton.setDisable(bool)
    optionsView.calculateButton.setDisable(bool)
    optionsView.cancelButton.setDisable(!bool)
  }
  def lockCalculate(lock : Boolean = true) : Unit = {
    optionsView.calculateButton.setDisable(lock)
   // optionsView.cancelButton.setDisable(lock)
  //  optionsView.enterButton.setDisable((!lock))
    optionsView.eraseButton.setDisable(!lock)
  }
  def setupMechanism() : Unit = {
    val observable : ObservableList[MechanismType] = FXCollections.observableArrayList(
      ExternalExternal,
      ExternalInternal,
      InternalInternal,
      InternalExternal,
      External1,
      Internal1)
    optionsView.mechanismTypeCombo.setItems(observable)
    optionsView.mechanismTypeCombo.getSelectionModel.select(0)
  }
  def setupCarrier() : Unit = {
    val observable : ObservableList[CarrierPosition] =
      FXCollections.observableArrayList(CarrierInput, CarrierOutput, CarrierNeutral)
    optionsView.carrierPosCombo.setItems(observable)
    optionsView.carrierPosCombo.getSelectionModel.select(0)
    //TODO carrier position combo box
  }
  def setupMode() : Unit = {
    val observable : ObservableList[ProcessType] = FXCollections.observableArrayList(KINEMATIC_ANALYSIS_FORWARD, KINEMATIC_SYNTHESIS, STRENGTH_ANALYSIS_FORWARD, STRENGTH_SYNTHESIS)
    optionsView.modeTypeCombo.setItems(observable)
    optionsView.modeTypeCombo.getSelectionModel.select(0)
  }

  override protected var observable: Observable = _

  override def onChange(event: Event): Unit = {
    event match  {
      case UnsuccessfulEnter =>
        lockCalculate(true)
      case SuccessfulEnter =>
        lockCalculate(false)
      case MechanismChangedEvent(_) => lockCalculate(true)
      case ModeChangedEvent(_) => lockCalculate(true)
      case CalculationStarted => setLock(true)
      case _ : CalculatingResultObtained =>
        setLock(false)
        lockCalculate(true)
      case InitialEvent() => lockCalculate(true)
      case _ => ()
    }
  }
}
abstract class AbstractOptionsViewControllerFactory(val location : String = "OptionsView.fxml") extends ViewFactory[AbstractOptionsViewControllerFactory]{
  override def getLocation : URL = {
    classOf[AbstractOptionsViewControllerFactory].getResource(location)
  }
}
import javafx.collections.FXCollections

class OptionsViewControllerFactory extends AbstractOptionsViewControllerFactory {
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[OptionsView]
    new OptionsViewController(controller)
  }
}
