package planar_interface.view

import planar_interface.{Event, MechanismController, MechanismControllerConcrete, Observable, Observer}
import planar_interface.view.event_types.{Calculate, CalculatingFinished, CalculatingResultObtained, CalculationStarted, EnterEvent, EraseEvent, InitialEvent, MechanismChangedEvent, MechanismConfigChanged, ModeChangedEvent}
import planar_interface.view.gear_view_behavior.{AbstractGearViewBehaviorFactory, GearViewBehavior, GearViewBehaviorFactory}
import planar_structure.mechanism.types._

/**
 * Changes current GearGroupView mode if notified that the menu changed
 * Takes the new mechanism in case it was changed and then adapts the corresponding
 * visual elements to the new mechanism
 * Then attaches new visual elements to the higher view class
 */
class GearViewMenu extends  Observer {
  /*protected var currentGearViewController : GearParamsInput = _
  protected var currentGearViewControllerFactory : AbstractGearGroupListViewControllerFactory = _*/
  protected var gearViewBehavior : GearViewBehavior = _
  protected val gearViewBehaviorFactory : AbstractGearViewBehaviorFactory = GearViewBehaviorFactory
  override protected var observable : Observable = MechanismControllerConcrete
  init()
  //TODO list with factories for controllers for every mode
  protected def observableController : MechanismController = observable.asInstanceOf[MechanismController]
  /**
   * @note for storing old view and reusability of old created views in order to not create new view for old modes and mechanisms
   */
  def init() : Unit = {
    gearViewBehavior = gearViewBehaviorFactory.apply(ProcessType.KINEMATIC_ANALYSIS_FORWARD)
  }

  def updateView(): Unit = {
    gearViewBehavior.updateView()
    //scrollPane.setContent(gearViewBehavior.getParent)
    /*import javafx.scene.control.ScrollPane.ScrollBarPolicy
    // Always show vertical scroll bar// Always show vertical scroll bar
    scrollPane.setVbarPolicy(ScrollBarPolicy.ALWAYS)
    // Horizontal scroll bar is only displayed when needed
    scrollPane.setHbarPolicy(ScrollBarPolicy.ALWAYS)
    BorderPane.setAlignment(scrollPane, Pos.TOP_LEFT)*/
    observableController.updateGearViewWith(gearViewBehavior.getParent)
  }

  private def onCalculatedCallback() : Unit = {
    observable.asInstanceOf[MechanismControllerConcrete.type].onChange(CalculatingFinished)
  }
  //TODO add suport for changing mode - should change used fabric
  override def onChange(event: Event): Unit = {
    event match {
      case EnterEvent => onEnter
      case ModeChangedEvent(processType) => changeMechanismView(processType) //TODO here the fabric should be changed also
      case CalculationStarted => blockView(true) //block view in case calculation is ignited
      case a : CalculatingResultObtained => blockView(false)
      case InitialEvent() => changeMechanismView(KINEMATIC_ANALYSIS_FORWARD)
      case EraseEvent => gearViewBehavior.clearInput
      case Calculate => {
        gearViewBehavior.calculateCB(onCalculatedCallback _ )

      }
      case _ => () //in this case do nothing, here we don't react to other event types
    }
  }
  protected def onEnter : Unit = {
    println("сработал ввод")
    if (gearViewBehavior.getCurrentController.isBlocked) {
      observableController.successfulEnter(false)
      gearViewBehavior.getCurrentController.unblockView
    }
    else
    if (gearViewBehavior.getCurrentController.checkInput){
      println("ввод принят")
    observableController.successfulEnter(true)
      gearViewBehavior.getCurrentController.performSideEffect()
      gearViewBehavior.getCurrentController.blockView
    }
    else{
      observableController.successfulEnter(false)
      println("ввод не принят")
    }
  }
  protected def blockView(block : Boolean) = {
    gearViewBehavior.blockView(block)
  }
  protected def changeMechanismView(new_mode : ProcessType) : Unit = {
    gearViewBehavior = gearViewBehaviorFactory(new_mode.toCode)
    updateView()
  }

}
