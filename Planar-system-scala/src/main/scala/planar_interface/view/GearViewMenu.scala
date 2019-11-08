package planar_interface.view

import javafx.scene.Parent
import planar_interface.{ControllerStatusReport, MechanismController, MechanismControllerConcrete, Observable, Observer}
import planar_interface.model._
import planar_interface.view.GearGroupListView.{AbstractGearGroupListViewControllerFactory, GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.OptionsView.{AbstractOptionsViewControllerFactory, OptionsViewController, OptionsViewControllerFactory}
import planar_structure.mechanism.Mechanism
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
  protected var currentGearViewController : GearGroupListViewController = _
  protected var currentGearViewControllerFactory : AbstractGearGroupListViewControllerFactory = _
  override protected var observable: Observable = _
  init()
  //TODO list with factories for controllers for every mode
  protected def observableController : MechanismController = observable.asInstanceOf[MechanismController]
  /**
   * @note for storing old view and reusability of old created views in order to not create new view for old modes and mechanisms
   */
  def init() : Unit = {
    currentGearViewControllerFactory = new GearGroupListViewControllerFactory
  }
  def updateView(): Unit = {
    observableController.updateGearViewWith(
      savedModes(observableController.getMechanism)(observableController.getMode).gearGroupListView.gearGroupList)
  }
  protected val savedModes : mutable.HashMap[Mechanism, mutable.HashMap[String, GearGroupListViewController]] =
    mutable.HashMap.empty[Mechanism, mutable.HashMap[String, GearGroupListViewController]]
  //TODO add suport for changing mode - should change used fabric
  override def onChange(): Unit = {
    //TODO add check if mode was changed - then we need to set a new fabric for the corresponding mode - so rework of this function is needed
    if (observable != null){
      val status = observable.getStatus.asInstanceOf[ControllerStatusReport]
      //if at least something has changed we must understand what to do
      if (status.mechanismChanged || status.modeChanged){
        val mechanism = observableController.getMechanism
        savedModes get mechanism match {
            //if we found an old mechanism then we must check if there is the necessary view for it
          case Some(value) => value get observableController.getMode match {
            case Some(view) =>
              println("Found the old view and the old mechanism")
              updateView()
            //in case we don't have such view yet we must create a new one
            case None => {
              println("Found the old mechanism but view")
              currentGearViewControllerFactory.setGearGroups(mechanism.characteristics.gearStructureCharacteristic.getGearGroups)
              //creating new view based on the mechanism of the observableController
              val new_view = currentGearViewControllerFactory
                .createView(observableController.getMechanism.characteristics.gearStructureCharacteristic.getGearGroups)
                .asInstanceOf[GearGroupListViewController]
              //now we must store the view, we are in this branch of case conditions thus the mechanism is already exist in our DB
              value.addOne(observableController.getMode -> new_view)
              updateView()
            }
          }
          //we didn't find such mechanism before
          case None => {
            println("Found nothing, baking the new ")
            val mechanism = observableController.getMechanism
            //creating new view based on the mechanism of the observableController
            val new_view = currentGearViewControllerFactory
              .createView(mechanism.characteristics.gearStructureCharacteristic.getGearGroups)
              .asInstanceOf[GearGroupListViewController]
            //adding an entrance for the new mechanism
            savedModes.addOne(mechanism -> mutable.HashMap.empty[String, GearGroupListViewController])
            savedModes(mechanism).addOne(observableController.getMode -> new_view)
            updateView()
          }
        }
      }
    }
  }
}
