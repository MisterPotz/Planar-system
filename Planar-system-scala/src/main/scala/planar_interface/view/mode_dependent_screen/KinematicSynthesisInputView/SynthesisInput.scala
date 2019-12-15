package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import javafx.scene.control.{ScrollPane, SplitPane}
import javafx.scene.layout.VBox
import javafx.scene.{Node, Parent}
import planar_interface.model.MechanismSynthesizer
import planar_interface.view.GearParamsInput
import planar_interface.{MechanismControllerConcrete, Observable}
import planar_interface.view.mode_dependent_screen.InputResultPairViewInterface
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_structure.mechanism.process.report.{FullConditionCheck, SynthesizedMechanisms}
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.CommonMechanismCharacteristics.WheelNumberArgs

class SynthesisInput extends InputResultPairViewInterface{
  override protected var observable: Observable = MechanismControllerConcrete
  private val splitPane : SplitPane = new SplitPane()
  var inputController : InputController = new InputController
  var resController : ResController = new ResController
  splitPane.getItems.addAll(inputController.getParent, resController.getParent)
  override def performSideEffect(): Unit = {
    inputController.performSideEffect()
  }

  /**
   *
   * result of any calculation must be inherited from Result trait
   */
  override def setResult(result: Option[InputResultPairViewInterface.Result]): Unit = {
    println("Setting result")
  }
  def calculate() : Unit = {
    inputController.calculate(resController.setResult _ )
  }
  def calculateCB(callback: () => Unit) : Unit = {
    calculate()
    callback()
  }

  /**
   * must be inherited from Argument trait, represents an input in a program
   */
  override def getArguments(): Option[InputResultPairViewInterface.Argument] = ???

  override def getRoot: Parent = splitPane

  override def getParent: Parent = splitPane

  override def clearInput(): Unit = inputController.clearInput()

  override def checkInput: Boolean = inputController.checkInput

}

/**
 * saves arguments from child views and gives the possibility to extract them and
 * pass to the outer scope
 */
trait ArgumentsAggregatorInterface{
  /**
   *
   * @return if an adequate input is given, collects and returns it
   *         otherwise side effect is performed - user is given a signal
   *         that the input is malformed
   */
  def extractArguments() : Option[InputResultPairViewInterface]

}

/**
 * view can be changed basing on some argument
 */
trait ChangeableView{
  def changeView()
}


class InputController extends GearParamsInput{
  protected val currentGearViewControllerFactory : KSIVC_Factory = new KSIVC_Factory()
  protected var controller : KinematicSynthesisInputViewController =
    currentGearViewControllerFactory.createView().asInstanceOf[KinematicSynthesisInputViewController]
  val rootScrollPane : ScrollPane = new ScrollPane()
  val root = controller.getParent
  rootScrollPane.setContent(root)

  def calculate(cb : (SynthesizedMechanisms) => Unit ) : Unit = {
    println("Calculated reports: here")

    val synthesized = MechanismSynthesizer.synthesizeByWheelNumber(WheelNumberArgs(controller.getU,controller.getAccuracyU, controller.getK))
    cb(synthesized)
  }
  override def getParent : Parent = rootScrollPane

  override def clearInput(): Unit = controller.clearInput()

  override def checkInput: Boolean = {
    controller.checkInput
  }

  override def performSideEffect(): Unit = {
    controller.performSideEffect()
  }
}

class ResController{
  protected val currentGearViewControllerFactory : ViewFactory[_] =
    new SynthesisResultViewControllerWFactory
  protected var controller : SynthesisResultViewControllerW =
    currentGearViewControllerFactory.createView().asInstanceOf[SynthesisResultViewControllerW]
  protected var rootScrollPane : ScrollPane = new ScrollPane()
  rootScrollPane.setContent(controller.getParent)
  def getParent : Node = rootScrollPane
  def setResult(res  : SynthesizedMechanisms) : Unit = {
    controller.obtainResults(res)
  }
}