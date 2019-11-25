package planar_interface.view.KinematicSynthesisInputView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.layout.VBox
import planar_interface.view.GearParamsInput
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.GearGroup

class GearGroupListView {
@FXML
  var gearGroupList : VBox = _
}

class GearGroupListViewController(var gearGroupListView: GearGroupListView,
                                  var controllers : List[GearGroupFullViewController])
extends GearParamsInput{
  init()
  def init() : Unit = {
    controllers.zipWithIndex.foreach((controller) => {
      val view : Parent = controller._1.getParent
      gearGroupListView.gearGroupList.getChildren.add(view)
      controller._1.gearGroupFullView.gearGroupLabel.setText(s"Группа зацеплений ${controller._2+1}")
    })
  }
  def deactivateViews(deactivate : Boolean) = {
    gearGroupListView.gearGroupList.setDisable(deactivate)
  }
  def enter() : Unit = {
    //must check all the fields for the good input
  }

  override def checkInput: Boolean = {
    controllers.foldLeft(true)((x,y ) => x & y.checkInput)
  }

  override def performSideEffect(): Unit = {
    controllers.foreach(x => x.performSideEffect())
  }

  override def getParent: Parent = gearGroupListView.gearGroupList

  override def clearInput(): Unit = {
    controllers.foreach(_.clearInput())
  }

  override def getUsefulObject: AnyRef = {
    controllers.map(_.getUsefulObject.asInstanceOf[GroupAndGearsViewParams])
  }
}


class GearGroupListViewControllerFactory(override val location: String = "GearGroupListView.fxml")
  extends ViewFactory[GearGroupListViewControllerFactory]{
  val gearGroupFullViewControllerFactory : GearGroupFullViewControllerFactory =
    new GearGroupFullViewControllerFactory
  var gearGroupsNumber : Int = 0
  var gearsPerGroup : Array[Int] = _
  def setGearGroupsNumber(some : Int ) :Unit = gearGroupsNumber = some
  def setGearsPerGroup(some : Array[Int]) :Unit = gearsPerGroup = some
  override def getLocation: URL = classOf[GearGroupListViewControllerFactory].getResource(location)
  override def createView(): AnyRef = {
    super.createView()
    val controller : GearGroupListView = this.controller.asInstanceOf[GearGroupListView]
    val gearGroupViewControllers = gearsPerGroup.map( (gearGroup) =>{
      gearGroupFullViewControllerFactory.setGearsNumber(gearGroup)
      gearGroupFullViewControllerFactory.createView().asInstanceOf[GearGroupFullViewController]
    }
    )
    new GearGroupListViewController(controller, gearGroupViewControllers.toList)
  }
}