package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import java.io.InputStream
import java.net.URL

import javafx.fxml.FXML
import javafx.geometry.Insets
import javafx.scene.Parent
import javafx.scene.control.{Accordion, Label, ScrollPane, TitledPane}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{GridPane, StackPane, VBox}
import javax.swing.ScrollPaneConstants
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.common_mechanisms.Common.MECHANISM_FULL_CLASSIFIER
import planar_structure.mechanism.process.report.{AdditionalInfoSynthesized, SynthesizedMechanisms}

import scala.collection.mutable.ListBuffer


class SynthesisResultViewController {
  @FXML
  var mechanismScrollPane: ScrollPane = _
  @FXML
  var variantsAmountLabel: Label = _
  @FXML
  var minimalSizeLabel: Label = _
  @FXML
  var resGridPane: GridPane = _
  @FXML
  var resultsAccordion: Accordion = _
  @FXML
  var schemeTypeLabel: Label = _
  @FXML
  var schemeImage: VBox = _
}

trait LocationTrait

class SynthesisResultViewControllerW(controller: SynthesisResultViewController) extends LocationTrait with HelpDouble {
  setAll("Расчет ещё не проведен")
  var paneControllers: List[TitledPaneViewControllerW] = null
  val factory: TitledPaneViewControllerWFactory.type = TitledPaneViewControllerWFactory

  def setAll(s: String = "") = {
    setVariantsAmount(s)
    setMinimalSizeLabel(s)
    setSchemeType(s)
    setSchemeVisibility(false)
  }

  def setSchemeVisibility(boolean: Boolean): Unit = {
    controller.schemeImage.setVisible(boolean)
  }

  def setVariantsAmount(some: String): Unit = {
    controller.variantsAmountLabel.setText(some)
  }

  def setMinimalSizeLabel(s: String): Unit = {
    controller.minimalSizeLabel.setText(s)
  }

  def setSchemeImage(stream: InputStream): Unit = {
    val image = new ImageView(new Image(stream))
    image.setPreserveRatio(true)
    image.setFitWidth(controller.resGridPane.getWidth * 0.4)
    if (controller.schemeImage.getChildren.size() > 0) {
      //ontroller.schemeImage.getChildren(
      controller.schemeImage.getChildren.add(image)
      setSchemeVisibility(true)
      image.setVisible(true)
    } else
      controller.schemeImage.getChildren.add(image)

  }

  //override protected var observable: Observable = _
  /*override def onChange(event: Event): Unit = {
    event match {
      case event: CalculatingResultObtained => obtainResults(event)
    }
  }*/
  def setSchemeType(s: String): Unit = {
    controller.schemeTypeLabel.setText(s)
  }

  def obtainResults(obj: AnyRef): Unit = {
    obj match {
      case some: SynthesizedMechanisms if some != null => setResults(some)
      case _ =>
        setAll("Ошибка при получении результатов")
      //kinematicForwardView.resultPane.setDisable(true)
    }
  }

  def setMechanismsListVisible(boolean: Boolean) : Unit = {
    controller.mechanismScrollPane.setVisible(boolean)
  }

  protected def setResults(check: SynthesizedMechanisms): Unit = {
    setMechanismsListVisible(true)
    val minimalSize: Double = check.minimalSize
    val mechsAmount: Int = check.mechanismAmount
    val minimal = check.minimalSize
    val type_ : String = check.mechClassifier.stringClassificator
    val image_stream = check.mechClassifier.PICTURE_PATH
    val list_ = check.sorted_mechanisms
    val additional = check.additionalInfo
    val target = check.u_target
    while (controller.resultsAccordion.getPanes.size() > 0) {
      controller.resultsAccordion.getPanes.remove(0)
    }
    setMinimalSizeLabel(minimalSize.toString)
    setVariantsAmount(mechsAmount.toString)
    setMinimalSizeLabel(minimal.toStringFormatted)
    setMechanisms(list_, check.mechClassifier.WHEEL_INDECES, additional,target)
    setSchemeType(type_)
    setSchemeVisibility(true)
    setSchemeImage(image_stream)

  }

  def setMechanisms(list: ListBuffer[Mechanism],
                    wheelIndeces: List[String],
                    additional : ListBuffer[AdditionalInfoSynthesized], targetU : Double): Unit = {
    //контроллеры вкладок на конечной вь/хе
    paneControllers = list.slice(0, 100).map(_ => factory
      .createView().asInstanceOf[TitledPaneViewControllerW]).toList
    paneControllers.zipWithIndex.foreach( paneController =>
      paneController._1.setMechanism(paneController._2, list(paneController._2),
        wheelIndeces,
        additional(paneController._2), targetU))
    paneControllers.foreach(c => controller.resultsAccordion.getPanes.add(c.parent))
    controller.resultsAccordion.setPrefWidth(paneControllers(0).parent.getPrefWidth + 100)
    /*controller.mechanismScrollPane.setPrefWidth(controller.resultsAccordion.getPrefWidth)*/
   // controller.resGridPane.setPrefWidth(paneControllers(0).parent.getPrefWidth + 100)
  }

  def setFailure(reason: String): Unit = {
    setAll(reason)
  }

  def getParent: Parent = controller.resGridPane

  def showLoading(boolean: Boolean): Unit = if (boolean) setAll("Результат вычисляется") else setAll("Результат получен")
}


class SynthesisResultViewControllerWFactory(override val location: String = "KinematicSynthesisResultView.fxml")
  extends ViewFactory[SynthesisResultViewControllerWFactory] {
  override def getLocation: URL = classOf[SynthesisResultViewControllerWFactory].getResource(location)

  override def createView(): AnyRef = {
    super.createView()
    new SynthesisResultViewControllerW(controller.asInstanceOf[SynthesisResultViewController])
  }
}