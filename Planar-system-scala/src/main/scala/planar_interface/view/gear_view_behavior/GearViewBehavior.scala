package planar_interface.view.gear_view_behavior

import javafx.scene.{Node, Parent}
import javafx.scene.control.{ComboBox, Label}
import javafx.scene.layout.VBox
import planar_interface.{Event, MechanismControllerConcrete, Observable, Observer}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupListView.{GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.GearParamsInput
import planar_interface.view.event_types.MechanismConfigChanged
import planar_interface.view.mode_dependent_screen.InputResultPairViewInterface
import planar_interface.view.mode_dependent_screen.kinematic_analysis.AnalysisInput
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.view.mode_dependent_screen.kinematic_analysis.singleton_mode_selector.SelectorSetupper
import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.{GearGroup, Mechanism}
import planar_structure.mechanism.types.{CarrierPosition, CodeGenerator, MechanismType}

trait GearViewBehavior extends Observable{
  addObserver(MechanismControllerConcrete)
  //val inputResultPairViewInterface : InputResultPairViewInterface

  /**
   * @return current input controller
   */
  def updateView() : Unit
  def getCurrentController : GearParamsInput
  def getCurrentFactory : ViewFactory[_]
  //initializes current controller
  //def installCurrentController(useful_object : AnyRef) : Unit
  def blockView(block : Boolean = false) : Unit
  def getParent : Parent
  def clearInput : Unit
  def calculate() : Unit
  def calculateCB(callback: () => Unit) : Unit = {}
}


object KinematicForwardBehavior extends GearViewBehavior with Observer{
   val inputResultPairView: AnalysisInput.type = AnalysisInput
  override def blockView(block: Boolean): Unit = {
    inputResultPairView.getRoot.setDisable(block)
    /*currentGearViewController.gearGroupListView.gearGroupList.setDisable(block)*/
  }
  override def getParent: Parent = {
    inputResultPairView.getRoot
  }

  /**
   * @return current input controller
   */
  override def getCurrentController: GearParamsInput = inputResultPairView

  override def getCurrentFactory: ViewFactory[_] = ???

  override def clearInput: Unit = inputResultPairView.clearInput()

  override protected var observable: Observable = _

  override def onChange(event: Event): Unit = {
    event match {
      case MechanismConfigChanged(mechanismType, carrierPos) => {
        inputResultPairView.inputController.installController()
      }
    }
  }

  /**
   * @return current input controller
   */
  override def updateView(): Unit = inputResultPairView.inputController.installController()

  override def calculate(): Unit = inputResultPairView.calculate()

  override def calculateCB(callback: () => Unit): Unit = inputResultPairView.calculateCB(callback)
}

/**
 * creates mechanism based on selected options
 */
class ModeController{
  private val setup : VBox = SelectorSetupper.vbox
  def getParent : Node = setup
  def getSelectedMechanism : (MechanismType, CarrierPosition) ={
    SelectorSetupper.getSelected
  }
  def createSelectedMechanism : Mechanism = {
    val selected = getSelectedMechanism
    Mechanism2KH(CodeGenerator(selected._1, selected._2))
  }
}