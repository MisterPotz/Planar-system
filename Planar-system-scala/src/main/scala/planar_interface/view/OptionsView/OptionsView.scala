package planar_interface.view.OptionsView

import java.net.URL

import planar_structure.mechanism.types._
import javafx.collections.{FXCollections, ObservableList}
import javafx.fxml.FXML
import javafx.scene.Node
import javafx.scene.control.{Button, ComboBox, MenuBar}
import javafx.scene.layout.{AnchorPane, BorderPane}
import planar_interface.{Event, Observable, Observer}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupView.AbstractGearGroupOnlyViewControllerFactory
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.view.event_types.{CalculatingResultObtained, CalculationStarted, InitialEvent, MechanismChangedEvent, ModeChangedEvent, SuccessfulEnter, UnsuccessfulEnter}

class OptionsView {

  @FXML
  var modeTypeCombo : ComboBox[ProcessType] = _
  @FXML
  var appBorderPane : BorderPane = _
  @FXML
  var eraseButton : Button = _
  @FXML
  var enterButton : Button = _
  @FXML
  var settingsAnchorPane : AnchorPane = _

  @FXML
  var appMenuBar : MenuBar =  _
  @FXML
  var calculateButton : Button = _
  @FXML
  var cancelButton : Button = _
}



class OptionsViewController(var optionsView: OptionsView) extends Observer {
  override protected var observable: Observable = _
  init()
  def init() : Unit = {
    setupMode();
    setLock(false)
  }
  def setLock(bool : Boolean) = {
    optionsView.modeTypeCombo.setDisable(bool)
    optionsView.eraseButton.setDisable(bool)
    optionsView.modeTypeCombo.setDisable(bool)
    optionsView.calculateButton.setDisable(bool)
    optionsView.cancelButton.setDisable(!bool)
  }
  def lockCalculate(lock : Boolean = true) : Unit = {
    optionsView.calculateButton.setDisable(lock)
   // optionsView.cancelButton.setDisable(lock)
  //  optionsView.enterButton.setDisable((!lock))
    optionsView.eraseButton.setDisable(!lock)
  }

  def setupMode() : Unit = {
    val observable : ObservableList[ProcessType] = FXCollections.observableArrayList(KINEMATIC_ANALYSIS_FORWARD, KINEMATIC_SYNTHESIS)
    optionsView.modeTypeCombo.setItems(observable)
    optionsView.modeTypeCombo.getSelectionModel.select(0)
  }

  def getMode() : ProcessType = {
    optionsView.modeTypeCombo.getSelectionModel.getSelectedItem
  }
  def setup(setupFunc : (ComboBox[ProcessType],
              Button,
              Button,
              Button) => Unit) : Unit = {
    setupFunc(optionsView.modeTypeCombo,
      optionsView.enterButton,
      optionsView.calculateButton,
      optionsView.eraseButton
    )
  }
  def setInputView(view : Node) : Unit = {
    optionsView.appBorderPane.setCenter(view)
  }
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
