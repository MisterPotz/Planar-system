package planar_interface.view.result

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.layout.GridPane
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.view.event_types.{CalculatedKinematicForward, CalculatingResultObtained}
import planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView.HelpDouble
import planar_interface.{Event, Observable, Observer}
import planar_structure.mechanism.process.report.{FullConditionCheck, KinematicAnalysisReport}
import planar_structure.mechanism.types.ConditionTypes

import scala.collection.immutable
trait HelpBoolean{
  implicit class HelpBooleanW(bool : Boolean){
    def prettyMeaning : String = {
      if (bool){
        "+"
      } else {
        "-"
      }
    }
  }
}

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
  @FXML
  var aw2Label : Label = null
  @FXML
  var awAccLabel : Label = _
  @FXML
  var maxSize : Label = _
}





class KinematicForwardViewController(val kinematicForwardView : KinematicForwardView)
  extends AbstractResultViewController with HelpDouble with HelpBoolean {
  setAll("Расчет ещё не проведен")
  def setAll(s : String = "") = {
    setGearRatio(s)
    //setAlignment(s)
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

  def setAw2Acc(s : String): Unit = {
    kinematicForwardView.awAccLabel.setText(s)
  }

  def setAw2(s : String): Unit = {
    kinematicForwardView.aw2Label.setText(s)
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
  def obtainResults(obj : KinematicAnalysisReport) : Unit = {
    obj match {
      case some : KinematicAnalysisReport => setResults(some)
      case _=>
        setAll("Ошибка при получении результатов")
        kinematicForwardView.resultPane.setDisable(true)
    }
  }
  protected def setResults(check: KinematicAnalysisReport) : Unit = {
    kinematicForwardView.resultPane.setDisable(false)
    val gearRatio = check.resU
    val aw2 = check.aw2.toStringFormatted + " мм"
    val accAw2 = check.alignmentAccuracy.toStringFormatted + " %"
    val maxSize = check.maximumSize.toStringFormatted + " мм"
    val neigh = check.neighborhood
    val assembly = check.assembly
    val pruning = check.pruning
    val pruningTemp = pruning.foldLeft(true)(_ & _)
    setGearRatio(gearRatio.toStringFormatted)
    //setNoInterference(check.interferenceAll.toString)
    setNoPruning(pruningTemp.prettyMeaning)
    setNeighborhood(neigh.prettyMeaning)
    setAssembly(assembly.prettyMeaning)
    setAw2(aw2)
    setAw2Acc(accAw2)
    setMaxSize(maxSize)
  }

  def setMaxSize(s : String): Unit = {
    kinematicForwardView.maxSize.setText(s)
  }

  def getResultsFromObservable() : immutable.HashMap[String, String] = {
    observable.asInstanceOf //TODO this one
  }
  def updateView() : Unit = {
    val results = getResultsFromObservable()
    setGearRatio(results(ConditionTypes.GEAR_RATIO))
    //setAlignment(results(ConditionTypes.ALIGNMENT))
    setAssembly(results(ConditionTypes.ASSEMBLY))
    setNoPruning(results(ConditionTypes.NO_PRUNING))
    setNeighborhood(results(ConditionTypes.NEIGHBORHOOD))
    setNoInterference(results(ConditionTypes.NO_INTERFERENCE))
  }



  override def getNode: Node = kinematicForwardView.resultPane

  override def showLoading(boolean: Boolean): Unit = if (boolean) setAll("Результат вычисляется") else setAll("Результат получен")

  override def clearResults(): Unit = {
    setAll("Расчет ещё не проведен")
  }
}


abstract class AbstractKinematicForwardViewControllerFactory(val location : String = "KinematicForwardView.fxml")
  extends ViewFactory[AbstractKinematicForwardViewControllerFactory]
{
  override def getLocation : URL = {
    classOf[AbstractKinematicForwardViewControllerFactory].getResource(location)
  }
}
import javafx.collections.FXCollections

object KinematicForwardViewControllerFactory extends AbstractKinematicForwardViewControllerFactory {
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[KinematicForwardView]
    new KinematicForwardViewController(controller)
  }
}




