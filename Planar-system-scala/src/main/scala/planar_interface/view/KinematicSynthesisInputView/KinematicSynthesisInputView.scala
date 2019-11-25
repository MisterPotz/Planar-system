package planar_interface.view.KinematicSynthesisInputView

import java.net.URL

import javafx.scene.control.{Label, TextField}
import javafx.fxml.FXML
import javafx.geometry.{HPos, Insets, Pos}
import javafx.scene.{Node, Parent}
import javafx.scene.layout.{AnchorPane, ColumnConstraints, GridPane, VBox}
import planar_interface.view.GearView.ViewFactory
import planar_interface.view.{GearParamsInput, TextCallbackCheckerSimple}
import planar_structure.mechanism.process.argument.{AdditionalWheelParams, KinematicSynthesisArgs}
import planar_structure.mechanism.types.MechanismType

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/*
class GridPaneOps(val gridPane : GridPane){
  def getColumnElements(row : Int) : List[Node] = {
    val list : ListBuffer[Node] = ListBuffer.empty[Node]
    val childen = gridPane.getChildren
    for (i <- Range(0, childen.size())){
      if (GridPane.getRowIndex(childen.get(i)) == row) {
        list.addOne(childen.get(i))
      }
    }
    list.toList
  }
  def getRows( row_number : List[Int]) : List[List[Node]] = {
    val total_list : ListBuffer[List[Node]] = ListBuffer.empty[List[Node]]
    val children = gridPane.getChildren
    for (i <- row_number){
      val new_row = getColumnElements(i)
      total_list.addOne(new_row)
    }
    total_list.toList
  }
}
*/

class Gear

class KinematicSynthesisInputView {
  @FXML
  var parametersListVBox : VBox = _
   /*@FXML
  var groupParametersPane : AnchorPane = _*/
  @FXML
  var kTextField : TextField = _
  @FXML
  var uTextField : TextField = _
  @FXML
  var upsilonTextField : TextField = _
}


class KinematicSynthesisInputViewController(val kinematicSynthesisInputView: KinematicSynthesisInputView,
val groupsController : GearGroupListViewController) extends GearParamsInput{
  kinematicSynthesisInputView.parametersListVBox.getChildren.add(groupsController.getParent)
  override def getUsefulObject: AnyRef = {
    val groupParams = groupsController.getUsefulObject.asInstanceOf[List[GroupAndGearsViewParams]]
    val k = kChecker.getText().toByte
    val u = uChecker.getText().toFloat
    val ups = upsilonChecker.getText().toShort
//ca : Float, ha: Float, x: Float, m : Float, alpha: Float, beta: Float
    //TODO добавить параметров от дополнительных полей
    KinematicSynthesisArgsPartial(groupParams.flatMap(params => params.gears.map(gear_params => (gear_params.zmin, gear_params.zmax))).toArray
      ,k, u, ups, groupParams.flatMap(params => params.gears.map(gear_params => {
        AdditionalWheelParams(gear_params.ca, gear_params.ha, gear_params.x, params.group.m, params.group.alpha, params.group.beta)
      })).toArray)
  }
  override def checkInput: Boolean = {
    groupsController.checkInput & kChecker.checkIfElseSet() & uChecker.checkIfElseSet() & upsilonChecker.checkIfElseSet()
  }
  val kChecker = TextCallbackCheckerSimple((a) => a.toInt > 0 && a.toInt < 10,
    () => kinematicSynthesisInputView.kTextField.getText,
    (s) => kinematicSynthesisInputView.kTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число",true ,() => kinematicSynthesisInputView.kTextField.getPromptText)
  val uChecker = TextCallbackCheckerSimple((a) => math.abs(a.toFloat) < 500,
    () => kinematicSynthesisInputView.uTextField.getText,
    (s) => kinematicSynthesisInputView.uTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не число",true ,() => kinematicSynthesisInputView.uTextField.getPromptText)
  val upsilonChecker = TextCallbackCheckerSimple((a) => a.toInt < 100 && a.toInt > 0,
    () => kinematicSynthesisInputView.upsilonTextField.getText,
    (s) => kinematicSynthesisInputView.upsilonTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число",true ,() => kinematicSynthesisInputView.upsilonTextField.getPromptText)
  //sets on textfield that it has wrong input
  override def performSideEffect(): Unit = ()

  override def blockView: Unit = {
    getParent.setDisable(true)
  }

  override def isBlocked: Boolean = {
    getParent.isDisabled
  }
  override def unblockView: Unit = {
    getParent.setDisable(false)
  }
  override def getParent: Parent = kinematicSynthesisInputView.parametersListVBox

  override def clearInput(): Unit = {
    kChecker.setText("")
    uChecker.setText("")
    upsilonChecker.setText("")
    groupsController.clearInput()
  }
}

class KSIVC_Factory(override val location : String = "KinematicSynthesisInputView.fxml")
  extends ViewFactory[KSIVC_Factory] {
  val gearGroupListViewControllerFactory : GearGroupListViewControllerFactory =
    new GearGroupListViewControllerFactory
  protected var mechanismType : MechanismType = null
  override def getLocation: URL  = classOf[KSIVC_Factory].getResource(location)
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): AnyRef= {
    super.createView() //updating loader
    val groupsAmount = mechanismType.groupsAmount
    val wheelInGroup = mechanismType.wheelsInGroup
    gearGroupListViewControllerFactory.setGearGroupsNumber(groupsAmount)
    gearGroupListViewControllerFactory.setGearsPerGroup(wheelInGroup.map(_.toInt))
    //создаем наш групповой контроллер
    val listViewController = gearGroupListViewControllerFactory.createView().asInstanceOf[GearGroupListViewController]
    //теперь её надо заполнить
    new KinematicSynthesisInputViewController(controller.asInstanceOf[KinematicSynthesisInputView], listViewController)
  }
  def setMechanismType(amount: MechanismType): Unit = mechanismType = amount
}

