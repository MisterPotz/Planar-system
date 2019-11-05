package planar_interface.view.GearGroupView

import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}
import planar_structure.mechanism.GearGroup
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, TextField}
import planar_structure.mechanism.GearWheel
import java.net.URL

import javafx.scene.Parent
import javafx.scene.layout.AnchorPane
import planar_interface.view.GearView.ViewFactory
class GearGroupOnlyView{
  @FXML
  var gearGroupOnlyPane : AnchorPane = _
  @FXML
  var mTextField : TextField = _
  @FXML
  var alphaTextField : TextField = _
  @FXML
  var betaTextField : TextField = _
}
class GearGroupOnlyViewController(var gearGroup : GearGroup, var gearGroupOnlyView: GearGroupOnlyView){
  protected def checkTextM(a : String) : Boolean = {
    try {
      if (a.toDouble > 0 && a.toDouble < 20) true else false
    }catch {
      case _ : Exception => false
    }
  }
  protected def checkTextAlpha(a : String) : Boolean = {
    try {if (a.toDouble < 50 && a.toDouble > 0) true else false}
    catch {
      case _ : Exception => false
    }
  }
  protected def checkTextBeta(a : String) : Boolean = {
    try {if (a.toDouble < 25 && a.toDouble > 0) true else false}
    catch {
      case _ : Exception => false
    }
  }
  protected def fullCondition(a : String) : Boolean = {
    //if !checkTextAlpha(a) сделать то-то и так далее по всем условиям
    checkTextAlpha(a) && checkTextBeta(a) && checkTextM(a) //TODO сделать подсветку неправильных полей
  }
}




abstract class AbstractGearGroupOnlyViewControllerFactory(val gearGroup: GearGroup,
                                                 val location : String = "GearGroupOnlyView.fxml") extends ViewFactory[AbstractGearGroupOnlyViewControllerFactory]{
  override def getLocation : URL = {
    classOf[AbstractGearGroupOnlyViewControllerFactory].getResource(location)
  }
}

class GearGroupOnlyViewControllerFactory(gearGroup: GearGroup) extends AbstractGearGroupOnlyViewControllerFactory(gearGroup){
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[GearGroupOnlyView]
    new GearGroupOnlyViewController(gearGroup, controller)
  }
}