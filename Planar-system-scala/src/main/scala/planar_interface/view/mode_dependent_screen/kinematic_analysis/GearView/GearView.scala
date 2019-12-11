package planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView

import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, TextField}
import planar_structure.mechanism.GearWheel
import java.net.URL

import scala.reflect.ClassTag
import javafx.scene.Parent
import javafx.scene.layout.GridPane
import planar_interface.view.{GearParamsInput, TextCallbackChecker, TextCallbackCheckerSimple}

class GearViewController(var gearWheel : GearWheel, var gearView: GearView) extends GearParamsInput{
  val zChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toInt > 5 && a.toInt < 400,
    () => gearView.zTextField.getText(), (s) => gearView.zTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число",true, () => gearView.zTextField.getPromptText)
  val xChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => math.abs(a.toDouble) < 5,
    () => gearView.xTextField.getText(), (s) => gearView.xTextField.setText(s),"Недопустимо большое смещение",
    "Введено не число",true ,() => gearView.xTextField.getPromptText)
  val haChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => (a.toDouble) < 2 && (a.toDouble) > 0 ,
    () => gearView.haTextField.getText(), (s) => gearView.haTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не число",true ,() => gearView.haTextField.getPromptText)
  val caChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => (a.toDouble) < 2 && (a.toDouble) > 0,
    () => gearView.caTextField.getText(), (s) => gearView.caTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не число",true ,() => gearView.caTextField.getPromptText)
  override def checkInput: Boolean = {
    zChecker.checkIfElseSet() & xChecker.checkIfElseSet() & haChecker.checkIfElseSet() &
    caChecker.checkIfElseSet()
  }

  override def performSideEffect(): Unit = {
    gearWheel.holder.z = zChecker.getText().toInt
    gearWheel.holder.x = xChecker.getText().toFloat
    gearWheel.holder.ha = haChecker.getText().toFloat
    gearWheel.holder.ca = caChecker.getText().toFloat
  }

  override def getParent: Parent = gearView.gearGridPane

  override def clearInput(): Unit = {
    zChecker.setText("")
    xChecker.setText("")
    caChecker.setText("")
    haChecker.setText("")
  }
}

class GearView{
  @FXML
  var gearNumberLabel : Label = _
  @FXML
  var gearGridPane : GridPane = _
  @FXML
  var zTextField : TextField = _
  @FXML
  var xTextField : TextField = _
  @FXML
  var haTextField : TextField = _
  @FXML
  var caTextField : TextField = _
}
trait ViewFactory[T <: AnyRef]{
  var loader : FXMLLoader = _
  val location : String
  def updateLoader() : Unit = {loader = new FXMLLoader(); loader.setLocation(getLocation)}
  def controller: AnyRef = loader.getController
  def getLocation : URL
  def parent(): Parent = loader.load()
  def createView() : AnyRef = {
    updateLoader()
    parent()
  }
}

abstract class AbstractGearViewControllerFactory(override val location : String = "GearView.fxml") extends ViewFactory[AbstractGearViewControllerFactory]{
  var gearWheel : GearWheel
  def setGearWheel(gearWheel: GearWheel) = this.gearWheel = gearWheel
  var gearNumber : Int = 1
  def setGearNumber(n : Int) : Unit = gearNumber = n
  override def getLocation: URL = {
    val a = classOf[AbstractGearViewControllerFactory].getResource(location)
    a
  }
}

//смысл в том чтобы каждый раз просто новый загрузчик делать. видимо там внутри есть какой-то механизм, который не
//позволяет еще раз взять и сделать новый объект, может оно и правильно для избежания ошибок
class GearViewControllerFactory extends AbstractGearViewControllerFactory{
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): GearViewController= {
    //updating loader for new instance of gearview
    super.createView()
    val controller = this.controller.asInstanceOf[GearView]
    controller.gearNumberLabel.setText(s"Колесо ${gearNumber}")
    new GearViewController(gearWheel, controller.asInstanceOf[GearView])
  }
  override var gearWheel: GearWheel = _
}