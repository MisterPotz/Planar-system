package planar_interface

import planar_interface.view.GearViewMenu

abstract  class AbstractMechanismControllerFactory{
  def createController() : MechanismController
}

class MechanismControllerFactory extends  AbstractMechanismControllerFactory {
  override def createController(): MechanismController = {
    val mechanismController = new MechanismControllerConcrete
    val gearViewMenu = new GearViewMenu
    //setting up proper observer
    mechanismController.addObserver(gearViewMenu)
    mechanismController.notifyObservers()
    mechanismController
  }
}