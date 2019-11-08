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

class GearViewController(var gearWheel : GearWheel, var gearView: GearView){
  protected def checkTextZ(a : String) : Boolean = {
    try {
      if (a.toInt > 5 && a.toInt < 400) true else false
    }catch {
      case _ : Exception => false
    }
  }
  protected def checkTextX(a : String) : Boolean = {
    try {if (math.abs(a.toDouble) < 100) true else false}
    catch {
      case _ : Exception => false
    }
  }
  def processAll() : Unit = {processEventX(); processEventZ();}
  def processEventZ() : Unit = {println(a)
    if (checkTextZ(gearView.zTextField.getText)){
      gearWheel.holder.z =  gearView.zTextField.getText.toInt
    }}
  def processEventX() : Unit  = {println(a)
    if (checkTextX(gearView.xTextField.getText)){
      gearWheel.holder.x =  gearView.xTextField.getText.toDouble
    }}
  var a : String = "Default gear"
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