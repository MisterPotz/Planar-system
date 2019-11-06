package planar_interface.view.OptionsView

import java.net.URL

import javafx.collections.{FXCollections, ObservableList}
import javafx.fxml.FXML
import javafx.scene.control.{Button, ComboBox, MenuBar}
import javafx.scene.layout.{AnchorPane, BorderPane}
import planar_interface.model.{CurrentMode, KINEMATIC_ANALYSIS_FORWARD, KINEMATIC_SYNTHESIS, STRENGTH_ANALYSIS_FORWARD, STRENGTH_SYNTHESIS}
import planar_interface.view.GearGroupView.AbstractGearGroupOnlyViewControllerFactory
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition}
import planar_structure.mechanism.mech2kh.MechanismType
import planar_structure.mechanism.mech2kh.{External1, ExternalExternal, ExternalInternal, Internal1, InternalExternal, InternalInternal}

class OptionsView {
  @FXML
  var carrierPosCombo : ComboBox[CarrierPosition] = _
  @FXML
  var modeTypeCombo : ComboBox[CurrentMode] = _
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
}
class OptionsViewController(var optionsView: OptionsView){
  init()
  def init() : Unit = {
    setupMode();
    setupMechanism()
    setupCarrier()
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
    val observable : ObservableList[CurrentMode] = FXCollections.observableArrayList(KINEMATIC_ANALYSIS_FORWARD, KINEMATIC_SYNTHESIS, STRENGTH_ANALYSIS_FORWARD, STRENGTH_SYNTHESIS)
    optionsView.modeTypeCombo.setItems(observable)
    optionsView.modeTypeCombo.getSelectionModel.select(0)
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
