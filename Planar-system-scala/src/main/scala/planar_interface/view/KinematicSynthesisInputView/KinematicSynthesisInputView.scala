package planar_interface.view.KinematicSynthesisInputView

import java.net.URL

import javafx.scene.control.{Label, TextField}
import javafx.fxml.FXML
import javafx.geometry.{HPos, Insets, Pos}
import javafx.scene.{Node, Parent}
import javafx.scene.layout.{ColumnConstraints, GridPane, VBox}
import planar_interface.view.GearView.ViewFactory
import planar_interface.view.{GearParamsInput, TextCallbackCheckerSimple}
import planar_structure.mechanism.process.actors.KinematicRatioFilteredActor.WheelParams
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.types.MechanismType

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

class Gear

class KinematicSynthesisInputView {
  @FXML
  var parametersListVBox : VBox = _
   @FXML
  var zminmaxGrid : GridPane = _
  @FXML
  var kTextField : TextField = _
  @FXML
  var uTextField : TextField = _
  @FXML
  var upsilonTextField : TextField = _
}

class WheelParamsInput(x : TextField, ca : TextField, ha : TextField) {
  //TODO implement checkers
}
class GroupParamsInput(val m : TextField, val alpha : TextField, val beta : TextField)
class GroupParametersViews(val labels : Array[Label],
                       val zminmax_arr : Array[(TextField, TextField)],
                      val wheel_params : Array[GridPane],
                      val group_params : GridPane)


class GroupOrganizer(val groupId : Byte, val pane : GridPane,val firstZNumber : Byte, val gearsInThisGroup : Byte){
  var zminmax_textfield : ArrayBuffer[(TextField, TextField)] = ArrayBuffer.empty[(TextField, TextField)]
  var wheel_params : ArrayBuffer[WheelParamsInput] = ArrayBuffer.empty[WheelParamsInput]
  var group_params : GroupParamsInput = _
  def new_textField : TextField = {
    val new_text = new TextField()
    setupTextField(new_text)
    new_text
  }
  def new_label(string : String) : Label = {
    val label = new Label(string)
    label.setMaxWidth(40)
    label
  }
  def setupTextField(text : TextField) : Unit = {
    GridPane.setMargin(text, new Insets(3,3,3,3))
    GridPane.setColumnSpan(text, 1)
    text.setMaxWidth(36)
  }
  def createInputsPerGroup() : GroupParametersViews = {
    val labels : Array[Label] = Array.fill(gearsInThisGroup){new Label()}
    labels.zipWithIndex.foreach{label =>
      label._1.setText(s"z${firstZNumber + label._2}")
    }
    //-----------------------------
    val zminmax_arr : Array[(TextField, TextField)] = Array.fill(gearsInThisGroup){(new_textField, new_textField)}
    //сохраняем поля ввода региона поиска чисел зубьев
    zminmax_arr.foreach(fields => zminmax_textfield.addOne(fields))
  //--------------------------
    val wheel_params : Array[GridPane] = Array.fill(gearsInThisGroup)({
      val box = new GridPane()
    /*  box.setMinWidth(box.getWidth)
      box.setMinHeight(box.getHeight)*/ //TODO these lines must be used later
      box.setAlignment(Pos.CENTER)
      box
    })
    //создаем и сохраняем поля индивидуального ввода колеса
    wheel_params.foreach(gridpane => {
      //добавляем лейблы и поля ввода
      gridpane.addColumn(0, new_label("x"), new_label("ca"), new_label("ha"))
      val x = new_textField
      val ca = new_textField
      val ha = new_textField
      gridpane.addColumn(1, x, ca, ha)
      val columnConstraints = new ColumnConstraints()
      columnConstraints.setHalignment(HPos.CENTER)
      columnConstraints.setFillWidth(true)
      gridpane.getColumnConstraints.add(columnConstraints)
      gridpane.getColumnConstraints.add(columnConstraints)
      this.wheel_params.addOne(new WheelParamsInput(x,ca, ha))
    })
    //-------------------------------------
    val geargroup_params : GridPane = new GridPane()
    GridPane.setRowSpan(geargroup_params, gearsInThisGroup.toInt)
    geargroup_params.addColumn(0, new_label("m"), new_label("α"), new_label("β"))
    val m = new_textField
    val alpha = new_textField
    val beta = new_textField
    geargroup_params.addColumn(1, m, alpha, beta)
    val columnConstraints = new ColumnConstraints()
    columnConstraints.setHalignment(HPos.CENTER)
    columnConstraints.setFillWidth(true)
    geargroup_params.getColumnConstraints.add(columnConstraints)
    geargroup_params.getColumnConstraints.add(columnConstraints)
    group_params = new GroupParamsInput(m,alpha, beta)
    new GroupParametersViews(labels,zminmax_arr,wheel_params,geargroup_params)
   }
}


class KinematicSynthesisInputViewController(val kinematicSynthesisInputView: KinematicSynthesisInputView,
val mechanismType: MechanismType) extends GearParamsInput{
  def grid : GridPane = kinematicSynthesisInputView.zminmaxGrid
  lazy val gearsNumber: Byte = mechanismType.wheelsAmount
  lazy val groupsNumber: Byte = mechanismType.groupsAmount
  lazy val wheelsInGroup : Array[Byte] = mechanismType.wheelsInGroup
  val groupOrganizers : Array[GroupOrganizer] = Range(1, groupsNumber+ 1).map(index => {
    val firstWheelIndex= (wheelsInGroup.slice(0, index-1).sum + 1).toByte
    val groupOrganizer = new GroupOrganizer(1, kinematicSynthesisInputView.zminmaxGrid,
      firstWheelIndex,wheelsInGroup(index-1))
    val input_set= groupOrganizer.createInputsPerGroup()
    for (i <- 0 until wheelsInGroup(index-1)){
      if (i == 0){
        grid.addRow(firstWheelIndex+i, input_set.labels(i), input_set.zminmax_arr(i)._1,
          input_set.zminmax_arr(i)._2, input_set.wheel_params(i),input_set.group_params)
      }
      else {
        grid.add(input_set.labels(i),0, firstWheelIndex+i)
        grid.add(input_set.zminmax_arr(i)._1,1, firstWheelIndex+i)
        grid.add(input_set.zminmax_arr(i)._2,2, firstWheelIndex+i)
        grid.add(input_set.wheel_params(i),3, firstWheelIndex+i)
      }
    }
    groupOrganizer
  }).toArray
  override def getUsefulObject: AnyRef = {

    val list : Array[(Short, Short)] = zminmaxInput.map(elem =>
      (elem._1.asInstanceOf[TextField].getText.toShort,
        elem._2.asInstanceOf[TextField].getText.toShort)).toArray
    val k = kChecker.getText().toByte
    val u = uChecker.getText().toFloat
    val ups = upsilonChecker.getText().toShort
    KinematicSynthesisArgsPartial(list, k, u, ups)
  }
  override def checkInput: Boolean = {
    checkAndSetZFields & kChecker.checkIfElseSet() & uChecker.checkIfElseSet() & upsilonChecker.checkIfElseSet()
  }
  val kChecker = TextCallbackCheckerSimple((a) => a.toInt > 0 && a.toInt < 10,
    () => kinematicSynthesisInputView.kTextField.getText,
    (s) => kinematicSynthesisInputView.kTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число")
  val uChecker = TextCallbackCheckerSimple((a) => math.abs(a.toFloat) < 500,
    () => kinematicSynthesisInputView.uTextField.getText,
    (s) => kinematicSynthesisInputView.uTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не число")
  val upsilonChecker = TextCallbackCheckerSimple((a) => a.toInt < 100 && a.toInt > 0,
    () => kinematicSynthesisInputView.upsilonTextField.getText,
    (s) => kinematicSynthesisInputView.upsilonTextField.setText(s),"Число вышло за допустимые пределы",
    "Введено не целое число")
  lazy val zminmaxInput : List[(Node, Node)] = {
    val gridPaneOps = new GridPaneOps(gridPane = kinematicSynthesisInputView.zminmaxGrid)
    val all_elements = gridPaneOps.getRows(Range(1, 1 + gearsNumber).toList)
    all_elements.map(row => {
      (row(1), row(2))}
    )
  }
  def checkAndSetZFields : Boolean = {
    zminmaxInput.foldLeft(true)((left, right) => {
      left & checkZPairField(right._1.asInstanceOf[TextField], right._2.asInstanceOf[TextField])
    })
    //checking all fields
  }
  def checkZField(z : TextField) : Boolean = {
    val textChecker = TextCallbackCheckerSimple((a) => a.toInt > 5 && a.toInt < 400,
      () => z.getText, (s) => z.setText(s),"Число вышло за допустимые пределы",
      "Введено не целое число")
    textChecker.checkIfElseSet()
  }
  def checkZPairField(z_min : TextField, z_max : TextField) : Boolean = {
    //если прошли первую проверку
    if (checkZField(z_min) & checkZField(z_max)){
      if (z_min.getText.toInt < z_max.getText.toInt){
        true
      }
      else
        false
    }
    else
      false
  }

  //sets on textfield that it has wrong input
  override def performSideEffect(): Unit = ()

  override def getParent: Parent = kinematicSynthesisInputView.parametersListVBox

  override def clearInput(): Unit = {
    kChecker.setText("")
    uChecker.setText("")
    upsilonChecker.setText("")
    zminmaxInput.foreach(a => {
      a._1.asInstanceOf[TextField].setText("")
      a._2.asInstanceOf[TextField].setText("")
    }
    )
  }
}

abstract class AbstractKSIVC_Factory(override val location : String = "KinematicSynthesisInputView.fxml")
extends ViewFactory[AbstractKSIVC_Factory] {
  def setMechanismType(amount: MechanismType)
  override def getLocation: URL  = classOf[AbstractKSIVC_Factory].getResource(location)
}


class KSIVC_Factory extends AbstractKSIVC_Factory {
  protected var mechanismType : MechanismType = null
  // Отображаем сцену, содержащую корневой макет.
  override def createView(): AnyRef= {
    super.createView() //updating loader
    //теперь её надо заполнить
    new KinematicSynthesisInputViewController(controller.asInstanceOf[KinematicSynthesisInputView], mechanismType)
  }

  override def setMechanismType(amount: MechanismType): Unit = mechanismType = amount
}