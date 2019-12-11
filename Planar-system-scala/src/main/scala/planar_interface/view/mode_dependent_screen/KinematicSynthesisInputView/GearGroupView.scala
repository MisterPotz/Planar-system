package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.control.TextField
import javafx.scene.layout.GridPane
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_interface.view.{GearParamsInput, TextCallbackChecker, TextCallbackCheckerSimple}
import planar_structure.mechanism.{GearGroup, GearGroupCommonParameters}

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
case class GroupViewParams(m : Float, alpha: Float, beta: Float)

class GearGroupOnlyViewController(var gearGroupOnlyView: GearGroupOnlyView) extends GearParamsInput{
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
  }

  override def getParent: Parent = gearGroupOnlyView.gearGroupOnlyPane

  override def clearInput(): Unit = {
    mChecker.setText("")
    alphaChecker.setText("")
    betaChecker.setText("")
  }

  override def getUsefulObject: AnyRef = {
    GroupViewParams(mChecker.getText().toFloat, alphaChecker.getText().toFloat, betaChecker.getText().toFloat)
  }
}


class GearGroupOnlyViewControllerFactory(val location : String = "GearGroupOnlyView.fxml") extends ViewFactory[GearGroupOnlyViewController]{
  override def getLocation : URL = {
    classOf[GearGroupOnlyViewControllerFactory].getResource(location)
  }
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[GearGroupOnlyView]
    new GearGroupOnlyViewController(controller)
  }
}