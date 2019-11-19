package planar_interface.view.GearGroupListView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.layout.VBox
import planar_interface.view.GearGroupView.{AbstractGearGroupFullViewControllerFactory, GearGroupFullViewController, GearGroupFullViewControllerFactory}
import planar_interface.view.GearParamsInput
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.GearGroup

class GearGroupListView {
@FXML
  var gearGroupList : VBox = _
}

class GearGroupListViewController(var gearGroupListView: GearGroupListView,
                                  var gearGroups : List[GearGroup],
                                  var controllers : List[GearGroupFullViewController])
extends GearParamsInput{
  init()
  def init() : Unit = {
    controllers.zipWithIndex.foreach((controller) => {
      val view : Parent = controller._1.gearGroupFullView.gearGroupAnchor
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
}

abstract class AbstractGearGroupListViewControllerFactory(override val location: String = "GearGroupListView.fxml")
  extends ViewFactory[AbstractGearGroupListViewControllerFactory]{
  override def getLocation: URL = classOf[AbstractGearGroupListViewControllerFactory].getResource(location)
  val gearGroupFullViewControllerFactory : AbstractGearGroupFullViewControllerFactory =
    new GearGroupFullViewControllerFactory
  def createView(gearGroups : List[GearGroup]) : AnyRef
  override def createView(): AnyRef = super.createView()
  var gearGroups : List[GearGroup] = _
  def setGearGroups(gearGroups : List[GearGroup]): Unit = this.gearGroups = gearGroups
}

class GearGroupListViewControllerFactory extends AbstractGearGroupListViewControllerFactory{
  override def createView(): AnyRef = {
    super.createView()
    val controller : GearGroupListView = this.controller.asInstanceOf[GearGroupListView]
    val gearGroupViewControllers = gearGroups.zipWithIndex.map( (gearGroup) =>{
      gearGroupFullViewControllerFactory.setGearGroup(gearGroup._1)
      gearGroupFullViewControllerFactory.createView().asInstanceOf[GearGroupFullViewController]
    }
    )
    new GearGroupListViewController(controller, gearGroups, gearGroupViewControllers )
  }
  override def createView(gearGroups : List[GearGroup]): AnyRef = {
    this.gearGroups = gearGroups
    createView()
  }
}