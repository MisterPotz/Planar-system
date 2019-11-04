package planar_interface.view

import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, TextField}
import planar_structure.mechanism.GearWheel
import java.net.URL

import javafx.scene.Parent

class GearViewController(var gearWheel : GearWheel, var zTextField : TextField, var xTextField : TextField
                        , var zButton : Button, var xButton : Button){
  setupHandlers()
  def setupHandlers() : Unit ={
    zTextField.setText("Почему эта нигга не работает?")
    zButton.setOnAction(eventHandlerZ)
    xButton.setOnAction(eventHandlerX)
  }
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
  def eventHandlerZ : EventHandler[ActionEvent] =  t => {println("Z")
    if (checkTextZ(zTextField.getText)){
      gearWheel.holder.z =  zTextField.getText.toInt
    }}
  def eventHandlerX : EventHandler[ActionEvent] = t => {println("X")
    if (checkTextX(xTextField.getText)){
      gearWheel.holder.x =  xTextField.getText.toDouble
    }}
}

class GearView{
  @FXML
  var zTextField : TextField = _
  @FXML
  var xTextField : TextField = _
  @FXML
  var zButton : Button = _
  @FXML
  var xButton : Button = _
}

abstract class AbstractGearViewControllerFactory(val gearWheel: GearWheel,
                                       val location : String = "GearView.fxml"){
  val loader : FXMLLoader = new FXMLLoader()
  loader.setLocation(getLocation)
  def createGearView() : GearViewController
  def getLocation : URL = {
    classOf[AbstractGearViewControllerFactory].getResource("GearView.fxml")
  }
  def getParent: Parent = {
    loader.load()
  }
}

class GearViewControllerFactory(gearWheel: GearWheel) extends AbstractGearViewControllerFactory(gearWheel){
  // Отображаем сцену, содержащую корневой макет.
  override def createGearView(): GearViewController= {
    val controller = loader.getController[GearView]
    new GearViewController(gearWheel, controller.zTextField, controller.xTextField, controller.zButton, controller.xButton)
  }

}