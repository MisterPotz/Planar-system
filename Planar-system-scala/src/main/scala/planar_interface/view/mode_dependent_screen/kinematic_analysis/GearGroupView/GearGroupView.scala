package planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupView

import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}
import planar_structure.mechanism.{GearGroup, GearGroupCommonParameters, GearWheel}
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, TextField}
import java.net.URL

import javafx.scene.Parent
import javafx.scene.layout.{AnchorPane, GridPane}
import planar_interface.view.{GearParamsInput, TextCallbackChecker, TextCallbackCheckerSimple}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
class GearGroupOnlyView{
  @FXML
  var gearGroupOnlyPane : GridPane = _
  @FXML
  var mTextField : TextField = _
  @FXML
  var alphaTextField : TextField = _
  @FXML
  var betaTextField : TextField = _
}
class GearGroupOnlyViewController(var gearGroup : GearGroup, var gearGroupOnlyView: GearGroupOnlyView) extends GearParamsInput{
  val mChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toDouble > 0 && a.toDouble < 20,
    () => gearGroupOnlyView.mTextField.getText(), (s) => gearGroupOnlyView.mTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не число", true, () => gearGroupOnlyView.mTextField.getPromptText)
  val alphaChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toDouble <= 50f && a.toDouble >= 0f,
    () => gearGroupOnlyView.alphaTextField.getText(), (s) => gearGroupOnlyView.alphaTextField.setText(s),
    "Число вышло за допустимые пределы (градусы)",
    "Введено не число",true, () => gearGroupOnlyView.alphaTextField.getPromptText)
  val betaChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toDouble <= 25 && a.toDouble >= 0,
    () => gearGroupOnlyView.betaTextField.getText(), (s) => gearGroupOnlyView.betaTextField.setText(s),
    "Число вышло за допустимые пределы (0 - 25 градусов)",
    "Введено не число",true, () => gearGroupOnlyView.betaTextField.getPromptText)

  override def checkInput: Boolean = {
    mChecker.checkIfElseSet() & alphaChecker.checkIfElseSet() & betaChecker.checkIfElseSet()
  }

  override def performSideEffect(): Unit = {
    val m = mChecker.getText().toFloat
    val alpha = alphaChecker.getText().toFloat.toRadians
    val beta = betaChecker.getText().toFloat.toRadians
    gearGroup.setCommon(GearGroupCommonParameters(m = m, alpha = alpha, beta = beta))
  }

  override def getParent: Parent = gearGroupOnlyView.gearGroupOnlyPane

  override def clearInput(): Unit = {
    mChecker.setText("")
    alphaChecker.setText("")
    betaChecker.setText("")
  }
}




abstract class AbstractGearGroupOnlyViewControllerFactory(val location : String = "GearGroupOnlyView.fxml") extends ViewFactory[AbstractGearGroupOnlyViewControllerFactory]{
  override def getLocation : URL = {
    classOf[AbstractGearGroupOnlyViewControllerFactory].getResource(location)
  }
  def setGearGroup(gearGroup: GearGroup) : Unit = this.gearGroup = gearGroup
  var gearGroup: GearGroup = _
}

class GearGroupOnlyViewControllerFactory extends AbstractGearGroupOnlyViewControllerFactory{
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[GearGroupOnlyView]

    new GearGroupOnlyViewController(gearGroup, controller)
  }
}