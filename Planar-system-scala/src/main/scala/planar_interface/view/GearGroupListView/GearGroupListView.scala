package planar_interface.view.GearGroupListView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.layout.VBox
import planar_interface.view.GearGroupView.{AbstractGearGroupFullViewControllerFactory, GearGroupFullViewController, GearGroupFullViewControllerFactory}
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.GearGroup

class GearGroupListView {
@FXML
  var gearGroupList : VBox = _
}

class GearGroupListViewController(var gearGroups : List[GearGroup], var gearGroupListView: GearGroupListView){

}

abstract class AbstractGearGroupListViewControllerFactory(override val location: String = "GearGroupListView.fxml")
  extends ViewFactory[AbstractGearGroupListViewControllerFactory]{
  override def getLocation: URL = classOf[AbstractGearGroupListViewControllerFactory].getResource(location)
  val gearGroupFullViewControllerFactory : AbstractGearGroupFullViewControllerFactory =
    new GearGroupFullViewControllerFactory
  def createView(gearGroups : List[GearGroup]) : AnyRef
  override def createView() = super.createView()
  var gearGroups : List[GearGroup] = _
  def setGearGroups(gearGroups : List[GearGroup]) = this.gearGroups = gearGroups
}

class GearGroupListViewControllerFactory extends AbstractGearGroupListViewControllerFactory{
  override def createView(): AnyRef = {
    super.createView()
    val curr_current : GearGroupListView = controller.asInstanceOf[GearGroupListView]
    gearGroups.zipWithIndex.foreach((gearGroup) => {
      gearGroupFullViewControllerFactory.setGearGroup(gearGroup._1)
      val lower_controller = gearGroupFullViewControllerFactory.createView().asInstanceOf[GearGroupFullViewController]
      val view : Parent = lower_controller.gearGroupFullView.gearGroupAnchor
      controller.asInstanceOf[GearGroupListView].gearGroupList.getChildren.add(view)
      lower_controller.gearGroupFullView.gearGroupLabel.setText(s"Группа зацеплений ${gearGroup._2+1}")
    })
    new GearGroupListViewController(gearGroups , controller.asInstanceOf[GearGroupListView])
  }
  override def createView(gearGroups : List[GearGroup]): AnyRef = {
    this.gearGroups = gearGroups
    createView()
  }
}