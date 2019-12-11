package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, TextField}
import planar_structure.mechanism.GearWheel
import java.net.URL

import scala.reflect.ClassTag
import javafx.scene.Parent
import javafx.scene.layout.GridPane
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.view.{GearParamsInput, TextCallbackChecker, TextCallbackCheckerSimple}

case class GearViewWheelParams(zmin : Short, zmax : Short, x : Float,ha : Float, ca : Float)

class GearViewController(var gearView: GearView, val number: Int) extends GearParamsInput{
  gearView.gearNumberLabel.setText(s"Колесо ${number+1}")
  val zminChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toInt > 5 && a.toInt < 400,
    () => gearView.zminTextField.getText(), (s) => gearView.zminTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число",true, () => gearView.zminTextField.getPromptText)
  val zmaxChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toInt > 5 && a.toInt < 400,
    () => gearView.zmaxTextField.getText(), (s) => gearView.zmaxTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число",true ,() => gearView.zmaxTextField.getPromptText)
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
    minmaxCheck() & xChecker.checkIfElseSet() & haChecker.checkIfElseSet() &
      caChecker.checkIfElseSet()
  }
  protected def minmaxCheck() : Boolean = {
    if (zminChecker.checkIfElseSet() & zmaxChecker.checkIfElseSet()){
      if (zminChecker.getText().toInt < zmaxChecker.getText().toInt){
        true
      }
      else {
        zminChecker.setText("min > max")
        false
      }
    }
    else
      false
  }
  override def getParent: Parent = gearView.gearGridPane

  override def clearInput(): Unit = {
    zminChecker.setText("")
    zmaxChecker.setText("")
    xChecker.setText("")
    caChecker.setText("")
    haChecker.setText("")
  }

  override def getUsefulObject: AnyRef = {
    GearViewWheelParams(zminChecker.getText().toShort, zmaxChecker.getText().toShort,
      xChecker.getText().toFloat, haChecker.getText().toFloat, caChecker.getText().toFloat)
  }
  override def performSideEffect(): Unit = ()
}

class GearView{
  @FXML
  var gearNumberLabel : Label = _
  @FXML
  var gearGridPane : GridPane = _
  @FXML
  var zmaxTextField : TextField = _
  @FXML
  var zminTextField : TextField = _
  @FXML
  var xTextField : TextField = _
  @FXML
  var haTextField : TextField = _
  @FXML
  var caTextField : TextField = _
}


//смысл в том чтобы каждый раз просто новый загрузчик делать. видимо там внутри есть какой-то механизм, который не
//позволяет еще раз взять и сделать новый объект, может оно и правильно для избежания ошибок
class GearViewControllerFactory(override val location : String = "GearView.fxml")  extends ViewFactory[GearViewControllerFactory]{
  var gearNumber : Short = 1
  def setGearNumber(some : Short) : Unit = gearNumber = some
  override def getLocation: URL = {
    val a = classOf[GearViewControllerFactory].getResource(location)
    a
  }
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): GearViewController= {
    //updating loader for new instance of gearview
    super.createView()
    new GearViewController(controller.asInstanceOf[GearView],gearNumber)
  }
}