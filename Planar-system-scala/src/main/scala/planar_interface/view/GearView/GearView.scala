package planar_interface.view.GearView

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
    "Введено не целое число")
  val xChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => math.abs(a.toDouble) < 5,
    () => gearView.xTextField.getText(), (s) => gearView.xTextField.setText(s),"Недопустимо большое смещение",
    "Введено не число")

  override def checkInput: Boolean = {
    zChecker.checkIfElseSet() & xChecker.checkIfElseSet()
  }

  override def performSideEffect(): Unit = {
    gearWheel.holder.z = zChecker.getText().toInt
    gearWheel.holder.x = xChecker.getText().toFloat
  }

  override def getParent: Parent = gearView.gearGridPane

  override def clearInput(): Unit = {
    zChecker.setText("")
    xChecker.setText("")
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
    new GearViewController(gearWheel, controller.asInstanceOf[GearView])
  }
  override var gearWheel: GearWheel = _
}