package planar_interface.view.mode_dependent_screen.kinematic_analysis

import javafx.scene.control.{ScrollPane, SplitPane}
import javafx.scene.layout.VBox
import javafx.scene.{Node, Parent}
import planar_interface.view.GearParamsInput
import planar_interface.view.gear_view_behavior.ModeController
import planar_interface.{MechanismControllerConcrete, Observable}
import planar_interface.view.mode_dependent_screen.InputResultPairViewInterface
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupListView.{GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.singleton_mode_selector.SelectorSetupper
import planar_interface.view.result.{AbstractKinematicForwardViewControllerFactory, KinematicForwardViewController, KinematicForwardViewControllerFactory}
import planar_structure.mechanism.process.report.FullConditionCheck
import planar_structure.mechanism.{Mechanism}

object AnalysisInput extends InputResultPairViewInterface{
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
  def calculateCB(callback : () => Unit) = {
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
  protected var controller : GearGroupListViewController = null
  protected val currentGearViewControllerFactory : GearGroupListViewControllerFactory = new GearGroupListViewControllerFactory
  protected var mechanism : Mechanism = null
  SelectorSetupper.installCB((some) => installController())
  val modeSelector: ModeController = new ModeController
  val rootScrollPane : ScrollPane = new ScrollPane()
  val root : VBox = new VBox()
  root.getChildren.addAll(modeSelector.getParent)
  rootScrollPane.setContent(root)
  var mechanismExtractor : () => Mechanism = modeSelector.createSelectedMechanism _
  def setMechanismExtractor(some : () => Mechanism) : Unit = {
    mechanismExtractor = some
  }
  def installController() : Unit = {
    if (mechanismExtractor == null){
      throw new IllegalStateException(s"${classOf[InputController].toString}: Mechanism type extractor was not set")
    }
    mechanism = mechanismExtractor()
    currentGearViewControllerFactory.setGearGroups(mechanism.getGearGroups)
    controller = currentGearViewControllerFactory.createView().asInstanceOf[GearGroupListViewController]
    updateView()
  }
  def calculate(cb : (FullConditionCheck) => Unit ) : Unit = {
    cb(mechanism.methods.fullConditionCheck)
  }
  def updateView() : Unit = {
    if (root.getChildren.size > 1 ){
      root.getChildren.remove(1)
    }
    root.getChildren.addAll(controller.getParent)
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
  protected val currentGearViewControllerFactory : AbstractKinematicForwardViewControllerFactory =
    KinematicForwardViewControllerFactory
  protected var controller : KinematicForwardViewController =
    currentGearViewControllerFactory.createView().asInstanceOf[KinematicForwardViewController]
  protected var rootScrollPane : ScrollPane = new ScrollPane()
  rootScrollPane.setContent(controller.getNode)
  def getParent : Node = rootScrollPane
  def setResult(res  : FullConditionCheck) : Unit = {
    controller.obtainResults(res)
  }
}