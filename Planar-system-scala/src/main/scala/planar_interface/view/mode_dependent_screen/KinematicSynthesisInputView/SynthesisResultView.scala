package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import java.net.URL

import javafx.fxml.FXML
import javafx.geometry.Insets
import javafx.scene.Parent
import javafx.scene.control.{Accordion, Label, TitledPane, TreeItem}
import javafx.scene.layout.GridPane
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.report.SynthesizedMechanisms


class SynthesisResultViewController {
  @FXML
  var variantsAmountLabel : Label  = _
  @FXML
  var minimalSizeLabel : Label = _
  @FXML
  var resGridPane : GridPane = _
  @FXML
  var resultsAccordion : Accordion = _
}

trait LocationTrait
class SynthesisResultViewControllerW(controller : SynthesisResultViewController) extends LocationTrait {
  setAll("Расчет ещё не проведен")
  def setAll(s : String = "") = {
    setVariantsAmount(s)
    setMinimalSizeLabel(s)
  }

  def setVariantsAmount(some : String): Unit = {
    controller.variantsAmountLabel.setText(some)
  }
  def setMinimalSizeLabel(s : String): Unit = {
    controller.minimalSizeLabel.setText(s)
  }

  //override protected var observable: Observable = _
  /*override def onChange(event: Event): Unit = {
    event match {
      case event: CalculatingResultObtained => obtainResults(event)
    }
  }*/

  def obtainResults(obj : AnyRef) : Unit = {
    obj match {
      case some : SynthesizedMechanisms if some != null => setResults(some)
      case _=>
        setAll("Ошибка при получении результатов")
        //kinematicForwardView.resultPane.setDisable(true)
    }
  }
  protected def setResults(check: SynthesizedMechanisms) : Unit = {
    val minimalSize : Int = check.minimalSize
    val mechsAmount : Int = check.mechanismAmount
    val list_  = check.sorted_mechanisms
    setMinimalSizeLabel(minimalSize.toString)
    setVariantsAmount(mechsAmount.toString)
    setMechanisms(list_)
  }
  def createOneMechanismsPane(index : Int, mech : Mechanism) : TitledPane = {
    val pane = new TitledPane()
    pane.setText(s"Набор $index")
    val grid = new GridPane()

    grid.setPadding(new Insets(5))
    grid.setHgap(5)
    grid.setVgap(1)

    val gearsAmount = mech.getGears.length
    for (i <- 0 until gearsAmount){
      grid.add(new Label(s"Колесо $i"), 0,i)
      grid.add(new Label(s"${mech.getGears(i).holder.z}"), 1, i)
    }
    pane.setContent(grid)
    pane
  }
  def setMechanisms(list: List[Mechanism]): Unit = {
    val panes : List[TitledPane] = list.slice(130,160).zipWithIndex.map(elem => {
      createOneMechanismsPane(elem._2, elem._1)
    }
    )

    panes.foreach(
      controller.resultsAccordion.getPanes.add(_)
    )
  }

   def getParent: Parent = controller.resGridPane

   def showLoading(boolean: Boolean): Unit = if (boolean) setAll("Результат вычисляется") else setAll("Результат получен")
}


class SynthesisResultViewControllerWFactory(override val location : String ="KinematicSynthesisResultView.fxml")
  extends ViewFactory[SynthesisResultViewControllerWFactory]{
  override def getLocation: URL = classOf[SynthesisResultViewControllerWFactory].getResource(location)

  override def createView(): AnyRef = {
    super.createView()
    new SynthesisResultViewControllerW(controller.asInstanceOf[SynthesisResultViewController])
  }
}