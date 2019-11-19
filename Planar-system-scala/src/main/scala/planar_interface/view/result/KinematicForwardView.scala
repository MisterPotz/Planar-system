package planar_interface.view.result

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.layout.GridPane
import planar_interface.view.GearView.ViewFactory
import planar_interface.view.event_types.{CalculatedKinematicForward, CalculatingResultObtained}
import planar_interface.{Event, Observable, Observer}
import planar_structure.mechanism.process.report.FullConditionCheck
import planar_structure.mechanism.types.ConditionTypes

import scala.collection.immutable


class KinematicForwardView {
 @FXML
  var resultPane : GridPane = null
  @FXML
  var gearRatioLabel : Label = null
  @FXML
  var alignmentLabel : Label = null
  @FXML
  var neighborhoodLabel : Label = null
  @FXML
  var pruningLabel : Label = null
  @FXML
  var interferenceLabel : Label = null
  @FXML
  var assemblyLabel : Label = null
}



class KinematicForwardViewController(val kinematicForwardView : KinematicForwardView) extends AbstractResultViewController {
  setAll("Расчет ещё не проведен")
  def setAll(s : String = "") = {
    setGearRatio(s)
    setAlignment(s)
    setAssembly(s)
    setNeighborhood(s)
    setNoInterference(s)
    setNoPruning(s)
  }

  def setGearRatio(some : String): Unit = {
    kinematicForwardView.gearRatioLabel.setText(some)
  }
  def setAlignment(s : String): Unit = {
    kinematicForwardView.alignmentLabel.setText(s)
  }
  def setAssembly(s : String): Unit = {
    kinematicForwardView.assemblyLabel.setText(s)
  }
  def setNeighborhood(s : String): Unit = {
    kinematicForwardView.neighborhoodLabel.setText(s)
  }
  def setNoPruning(s : String): Unit = {
    kinematicForwardView.pruningLabel.setText(s)
  }
  def setNoInterference(s : String): Unit = {
    kinematicForwardView.interferenceLabel.setText(s)
  }

  override protected var observable: Observable = _
  override def onChange(event: Event): Unit = {
    event match {
      case event: CalculatingResultObtained => obtainResults(event)
    }
  }
  protected def obtainResults(obj : CalculatingResultObtained) : Unit = {
    obj match {
      case CalculatedKinematicForward(result) => setResults(result)
      case _ =>
        setAll("Ошибка при получении результатов")
        kinematicForwardView.resultPane.setDisable(true)
    }
  }
  protected def setResults(check: FullConditionCheck) : Unit = {
    kinematicForwardView.resultPane.setDisable(false)
    setGearRatio(check.gearRatio.toString)
    setNoInterference(check.interferenceAll.toString)
    setNoPruning(check.noPruning.toString)
    setNeighborhood(check.neighborhood.toString)
    setAssembly(check.assembly.toString)
    setAlignment(check.alignment.toString)
  }
  def getResultsFromObservable() : immutable.HashMap[String, String] = {
    observable.asInstanceOf //TODO this one
  }
  def updateView() : Unit = {
    val results = getResultsFromObservable()
    setGearRatio(results(ConditionTypes.GEAR_RATIO))
    setAlignment(results(ConditionTypes.ALIGNMENT))
    setAssembly(results(ConditionTypes.ASSEMBLY))
    setNoPruning(results(ConditionTypes.NO_PRUNING))
    setNeighborhood(results(ConditionTypes.NEIGHBORHOOD))
    setNoInterference(results(ConditionTypes.NO_INTERFERENCE))
  }

  override def getNode: Node = kinematicForwardView.resultPane

  override def showLoading(boolean: Boolean): Unit = if (boolean) setAll("Результат вычисляется") else setAll("Результат получен")
}


abstract class AbstractKinematicForwardViewControllerFactory(val location : String = "KinematicForwardView.fxml")
  extends ViewFactory[AbstractKinematicForwardViewControllerFactory]
{
  override def getLocation : URL = {
    classOf[AbstractKinematicForwardViewControllerFactory].getResource(location)
  }
}
import javafx.collections.FXCollections

class KinematicForwardViewControllerFactory extends AbstractKinematicForwardViewControllerFactory {
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[KinematicForwardView]
    new KinematicForwardViewController(controller)
  }
}




