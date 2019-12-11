package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.control.{Label, SplitPane}
import javafx.scene.layout.{AnchorPane, GridPane, VBox}
import planar_interface.view.GearParamsInput
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory

class GearGroupFullView {
  @FXML
  var gearGroupAnchor : GridPane  = _
  @FXML
  var gearGroupLabel : Label = _
  @FXML
  var gearsPane : VBox = _
  @FXML
  var groupsPane : VBox = _
}

case class GroupAndGearsViewParams(gears : List[GearViewWheelParams], group : GroupViewParams)

//must save the underlying lower-level controllers
class GearGroupFullViewController(var gearGroupFullView: GearGroupFullView,
                                  var gearListViewController : GearListViewController,
                                  var gearGroupOnlyViewController: GearGroupOnlyViewController
                                  ) extends GearParamsInput{
  init()
  def init() : Unit = {
    gearGroupFullView.gearsPane.getChildren.add(gearListViewController.getParent)
    gearGroupFullView.groupsPane.getChildren.add(gearGroupOnlyViewController.getParent)
  }
  //must get the common pane and fill it with gears of group on the left and common parameters for the group on the right
  //TODO understand what methods should be here
  override def checkInput: Boolean = {
    gearListViewController.checkInput & gearGroupOnlyViewController.checkInput
  }
  override def performSideEffect(): Unit = {
    gearListViewController.performSideEffect()
    gearGroupOnlyViewController.performSideEffect()
  }

  override def getUsefulObject: AnyRef = {
    GroupAndGearsViewParams(gearListViewController.getUsefulObject.asInstanceOf[List[GearViewWheelParams]],
      gearGroupOnlyViewController.getUsefulObject.asInstanceOf[GroupViewParams])
  }
  override def getParent: Parent = gearGroupFullView.gearGroupAnchor

  override def clearInput(): Unit = {
    gearListViewController.clearInput()
    gearGroupOnlyViewController.clearInput()
  }
}



class GearGroupFullViewControllerFactory(val location : String = "GearGroupFullView.fxml") extends ViewFactory [GearGroupFullViewControllerFactory]{
  val gearGroupOnlyViewControllerFactory : GearGroupOnlyViewControllerFactory = new GearGroupOnlyViewControllerFactory
  val gearListViewControllerFactory : GearListViewControllerFactory = new GearListViewControllerFactory
  def getLocation : URL = {
    classOf[GearGroupFullViewControllerFactory].getResource(location)
  }
  var gearsNumber : Int = 0
  def setGearsNumber(some : Int) : Unit =  gearsNumber = some
  override def createView(): AnyRef = {
    //обновляем загрузчик для сохранения текущего контрллера и вида
    super.createView()
    //текущий контроллер
    val curr_controller = controller.asInstanceOf[GearGroupFullView]
    gearListViewControllerFactory.setGearsNumber(gearsNumber.toShort)
    //теперь туда надо забросить то что мы полуаем от других фабрик
    val left = gearListViewControllerFactory.createView().asInstanceOf[GearListViewController]
    val right = gearGroupOnlyViewControllerFactory.createView().asInstanceOf[GearGroupOnlyViewController]
    //создаем новую штуку
    new GearGroupFullViewController(curr_controller, left, right)
  }
}
