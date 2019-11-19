package planar_interface.view

import javafx.geometry.Pos
import javafx.scene.Parent
import javafx.scene.layout.BorderPane
import planar_interface.{Event, MechanismController, MechanismControllerConcrete, Observable, Observer}
import planar_interface.model._
import planar_interface.view.GearGroupListView.{AbstractGearGroupListViewControllerFactory, GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.GearView.ViewFactory
import planar_interface.view.KinematicSynthesisInputView.KinematicSynthesisArgsPartial
import planar_interface.view.OptionsView.{AbstractOptionsViewControllerFactory, OptionsViewController, OptionsViewControllerFactory}
import planar_interface.view.event_types.{ArgumentGetterEvent, CalculatingResultObtained, CalculationStarted, EnterEvent, EraseEvent, InitialEvent, MechanismChangedEvent, ModeChangedEvent}
import planar_interface.view.gear_view_behavior.{AbstractGearViewBehaviorFactory, GearViewBehavior, GearViewBehaviorFactory}
import planar_structure.mechanism.{GearGroup, Mechanism}
import planar_structure.mechanism.types._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


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
  override protected var observable: Observable = _
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
   /* observableController.updateGearViewWith(
      savedModes(observableController.getMechanism)(observableController.getMode).getParent)*/
    BorderPane.setAlignment(gearViewBehavior.getParent, Pos.TOP_LEFT)
    observableController.updateGearViewWith(gearViewBehavior.getParent)
  }
  protected val savedModes : mutable.HashMap[Mechanism, mutable.HashMap[String, GearParamsInput]] =
    mutable.HashMap.empty[Mechanism, mutable.HashMap[String, GearParamsInput]]

  //TODO add suport for changing mode - should change used fabric
  override def onChange(event: Event): Unit = {
    event match {
      case EnterEvent => onEnter
      case ModeChangedEvent(processType) => changeMechanismView //TODO here the fabric should be changed also
      case CalculationStarted => blockView(true) //block view in case calculation is ignited
      case a : CalculatingResultObtained => blockView(false)
      case MechanismChangedEvent(new_mech) => changeMechanismView
      case InitialEvent() => changeMechanismView
      case EraseEvent => gearViewBehavior.getCurrentController.clearInput()
      case ArgumentGetterEvent(bundle) => {
        val args = gearViewBehavior.getCurrentController.getUsefulObject.asInstanceOf[KinematicSynthesisArgsPartial]
        bundle.bundle = args
      }
      case _ => () //in this case do nothing, here we don't react to other event types
    }
  }
  protected def onEnter : Unit = {
    println("сработал ввод")
    if (gearViewBehavior.getCurrentController.checkInput){
      println("ввод принят")
      observableController.successfulEnter(true)
      gearViewBehavior.getCurrentController.performSideEffect()
    }
    else{
      observableController.successfulEnter(false)
      println("ввод не принят")
    }
  }
  protected def blockView(block : Boolean) = {
    gearViewBehavior.blockView(block)
  }
  protected def changeMechanismView : Unit = {
    /*val mechanism = observableController.getMechanism
    savedModes get mechanism match {
      //if we found an old mechanism then we must check if there is the necessary view for it
      case Some(value) => value get observableController.getMode match {
        case Some(view) =>
          println("Found the old view and the old mechanism")
        //in case we don't have such view yet we must create a new one
        case None => {
          println("Found the old mechanism but view")
          gearViewBehavior.installCurrentController(mechanism.gearStructureCharacteristic.getGearGroups)
          //creating new view based on the mechanism of the observableController
          val new_view = createView
          //now we must store the view, we are in this branch of case conditions thus the mechanism is already exist in our DB
          value.addOne(observableController.getMode -> new_view)
        }
      }
      //we didn't find such mechanism before
      case None => {
        println("Found nothing, baking the new ")
        val mechanism = observableController.getMechanism
        //creating new view based on the mechanism of the observableController
        val new_view = createView
        //adding an entrance for the new mechanism
        savedModes.addOne(mechanism -> mutable.HashMap.empty[String, GearParamsInput])
        savedModes(mechanism).addOne(observableController.getMode -> new_view)
      }
    }*/
    gearViewBehavior = gearViewBehaviorFactory(observableController.getMode)
    installBehavior()
    updateView()
  }
  def installBehavior() : Unit = {
    observableController.getMode match {
      case ProcessType.KINEMATIC_ANALYSIS_FORWARD =>
        gearViewBehavior.installCurrentController(observableController.getMechanism.gearStructureCharacteristic.getGearGroups)
      case ProcessType.KINEMATIC_SYNTHESIS =>
        gearViewBehavior.installCurrentController(observableController.getMechanismType)
    }
  }
}
