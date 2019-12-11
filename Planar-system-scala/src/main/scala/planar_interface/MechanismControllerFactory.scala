package planar_interface

import planar_interface.view.event_types.{InitialEvent, MechanismChangedEvent}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.singleton_mode_selector.{ SelectorSetupper}
import planar_interface.view.{GearViewMenu}

abstract class AbstractMechanismControllerFactory{
  def createController() : MechanismController
}

object MechanismControllerFactory extends AbstractMechanismControllerFactory {
  override def createController(): MechanismController = {
    val mechanismController =  MechanismControllerConcrete
    val gearViewMenu = new GearViewMenu
   // val resultsViewMenu = new ResultsViewMenu
    //setting up proper observer
    mechanismController.addObserver(gearViewMenu)
   // mechanismController.addObserver(resultsViewMenu)
    mechanismController.notifyObservers(InitialEvent())
    mechanismController
  }
}