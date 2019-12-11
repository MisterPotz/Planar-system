package planar_interface.view.mode_dependent_screen.kinematic_analysis.singleton_mode_selector

import java.util.ResourceBundle

import com.sun.tools.javac.util.Log
import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.control.{ComboBox, Label}
import javafx.scene.layout.VBox
import planar_interface.Observable
import planar_interface.view.event_types.{CarrierPositionChanged, MechanismChangedEvent, MechanismConfigChanged}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.singleton_mode_selector.PairLabelCombo
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition, External1, ExternalExternal, ExternalInternal, Internal1, InternalExternal, InternalInternal, MechanismType}

object SelectorSetupper extends Observable {
   val mechanismTypePair = PairLabelCombo(new Label(ResourceBundle.getBundle("MechanismNames").getString("select_mechanism_type")),
    new ComboBox[MechanismType]())
   val carrierPosPair = PairLabelCombo(new Label(ResourceBundle.getBundle("MechanismNames").getString("select_carrier_position")),
    new ComboBox[CarrierPosition]())
  setupMechanismTypeView()
  setupCarrier()
  lazy val vbox: VBox = setChilden
  var CB : (MechanismConfigChanged) => Unit = null
  def installCB(cb : (MechanismConfigChanged) => Unit) : Unit = {
    CB = cb
    setupCB()
  }
  def setupCB() : Unit = {
    if (CB == null){
      throw new IllegalStateException(s"${SelectorSetupper.getClass.toString} callback is not initialized")
    }
    carrierPosPair.combo.setOnAction(event => {
      CB(MechanismConfigChanged(mechanismTypePair.combo.getSelectionModel.getSelectedItem,
        carrierPosPair.combo.getSelectionModel.getSelectedItem))
    })
    mechanismTypePair.combo.setOnAction(event => {
      CB(MechanismConfigChanged(mechanismTypePair.combo.getSelectionModel.getSelectedItem,
        carrierPosPair.combo.getSelectionModel.getSelectedItem))
    })
  }
  def setupMechanismTypeView() : ComboBox[MechanismType] = {
    val observable : ObservableList[MechanismType] = FXCollections.observableArrayList(
      ExternalExternal,
      ExternalInternal,
      InternalInternal,
      InternalExternal,
      External1,
      Internal1)
    setupPairCombo(observable, mechanismTypePair.combo)
    mechanismTypePair.combo
  }
  def setupCarrier() : ComboBox[CarrierPosition] = {
    val observable : ObservableList[CarrierPosition] =FXCollections.observableArrayList(
      CarrierInput, CarrierOutput, CarrierNeutral
      )
    setupPairCombo(observable,
      carrierPosPair.combo)
    carrierPosPair.combo
  }
  def setupPairCombo[T](observableList: ObservableList[T], comboBox: ComboBox[T]) : Unit =  {
    comboBox.setItems(observableList)
    comboBox.getSelectionModel.select(0)
  }
  def setChilden : VBox = {
    val box = new VBox()
    box.getChildren.addAll(mechanismTypePair.label, mechanismTypePair.combo, carrierPosPair.label, carrierPosPair.combo)
    box
  }
  def getSelected : (MechanismType, CarrierPosition)  = {
    (mechanismTypePair.combo.getSelectionModel.getSelectedItem, carrierPosPair.combo.getSelectionModel.getSelectedItem)
  }
}
