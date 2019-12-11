package planar_interface.view

import planar_interface.view.event_types.{CalculatedKinematicForward, CalculatingResultObtained, CalculationStarted, InitialEvent, ModeChangedEvent}
import planar_interface.view.result.{AbstractResultViewController, AbstractResultViewControllerFactory, ResultViewControllerFactory}
import planar_interface.{Event, MechanismController, Observable, Observer}
import planar_structure.mechanism.types.{KINEMATIC_ANALYSIS_FORWARD, ProcessType}

/*
class ResultsViewMenu extends Observer with Observable{
  //by default it should block the results view
  override protected var observable: Observable = _
  def observableController : MechanismController = observable.asInstanceOf[MechanismController]
  //universal factory for result views
  var resultViewControllersFactory : AbstractResultViewControllerFactory =
    ResultViewControllerFactory
  var currentResultView : AbstractResultViewController = resultViewControllersFactory.createResultViewController(
    ProcessType.KINEMATIC_ANALYSIS_FORWARD
  ).get
  override def onChange(event : Event): Unit = {
    event match {
      case obj : CalculatingResultObtained =>
        //showCalculationLoading(false)//unblock the result view and write the results
        checkResults(obj)
      case CalculationStarted => showCalculationLoading(true) //show that the result is currently being calculated
      case ModeChangedEvent(processType) => handleModeChange() //set and block the results view of current mode
      case InitialEvent() => changeView() //the same
      case _ => () //do nothing
    }
  }
  def showCalculationLoading(show : Boolean = true) : Unit = {
    currentResultView.showLoading(show)
  }
  //TODO later it will be necessary to pass mechanism type in the result constructor (probably, for more fluid interface)
  def changeView(): Unit = {
    val optioned = resultViewControllersFactory.createResultViewController(observableController.getMode)
    optioned match {
      case Some(view) => {
        currentResultView = view
        addObserver(currentResultView)
        updateResultView()
      }
      case None => println("Cannot create result view")
    }
  }
  def handleModeChange() : Unit = {
    changeView()
  }
  protected def updateResultView() :Unit = {
    //setting the result view
    observableController.getBorder.setRight(currentResultView.getNode)
  }
  //send the event to the listeners
  def checkResults(obj : CalculatingResultObtained) : Unit = {
     notifyObservers(obj)
  }
}*/
