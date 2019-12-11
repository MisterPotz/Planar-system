package planar_interface.view.mode_dependent_screen.kinematic_analysis.singleton_mode_selector

import java.util.ResourceBundle

import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.Parent
import javafx.scene.control.{ComboBox, Label}
import javafx.scene.layout.VBox
import planar_interface.model.MechanismDatabase
import planar_interface.view.GearParamsInput
import planar_interface.view.event_types.{CalculatingResultObtained, CalculationStarted, EraseEvent, InitialEvent, MechanismChangedEvent, ModeChangedEvent, SuccessfulEnter, UnsuccessfulEnter}
import planar_interface.view.gear_view_behavior.GearViewBehavior
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupListView.{GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.{Event, Observable, Observer}
import planar_structure.mechanism.GearGroup
import planar_structure.mechanism.types.{CarrierInput, CarrierNeutral, CarrierOutput, CarrierPosition, External1, ExternalExternal, ExternalInternal, Internal1, InternalExternal, InternalInternal, MechanismType}
case class PairLabelCombo[T](label : Label, combo: ComboBox[T])
