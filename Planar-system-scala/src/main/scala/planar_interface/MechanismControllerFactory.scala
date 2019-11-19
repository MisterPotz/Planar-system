package planar_interface

import planar_interface.view.event_types.{InitialEvent, MechanismChangedEvent}
import planar_interface.view.{GearViewMenu, ResultsViewMenu}

abstract  class AbstractMechanismControllerFactory{
  def createController() : MechanismController
}

class MechanismControllerFactory extends  AbstractMechanismControllerFactory {
  override def createController(): MechanismController = {
    val mechanismController = new MechanismControllerConcrete
    val gearViewMenu = new GearViewMenu
    val resultsViewMenu = new ResultsViewMenu
    //setting up proper observer
    mechanismController.addObserver(gearViewMenu)
    mechanismController.addObserver(resultsViewMenu)
    mechanismController.notifyObservers(InitialEvent())
    mechanismController
  }
}