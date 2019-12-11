package planar_interface.view.gear_view_behavior

import javafx.scene.Parent
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupListView.{GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.GearParamsInput
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView.{KSIVC_Factory, KinematicSynthesisInputViewController, SynthesisInput}
import planar_structure.mechanism.GearGroup
import planar_structure.mechanism.raw_algorithms.ExternalConnectionCalculation
import planar_structure.mechanism.types.{ExternalInternal, MechanismType}

object KinematicSynthesisBehavior extends GearViewBehavior{
  protected val inputResController: SynthesisInput = new SynthesisInput
  override def getCurrentController: GearParamsInput = inputResController.inputController

  override def blockView(block: Boolean): Unit = {
    inputResController.getParent.setDisable(block)
  }
  override def getParent: Parent = {
    inputResController.getParent
  }

  override def clearInput: Unit = inputResController.clearInput()

  /**
   * @return current input controller
   */
  override def updateView(): Unit = ()

  override def calculate(): Unit = ()

  override def getCurrentFactory: ViewFactory[_] = ???

  override def calculateCB(callback: () => Unit): Unit = {
    inputResController.calculateCB(callback)
  }
}
