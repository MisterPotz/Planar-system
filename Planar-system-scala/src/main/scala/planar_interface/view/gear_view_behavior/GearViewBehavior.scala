package planar_interface.view.gear_view_behavior

import javafx.scene.Parent
import planar_interface.view.GearGroupListView.{GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.GearParamsInput
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.GearGroup

trait GearViewBehavior{
  def getCurrentController : GearParamsInput
  def getCurrentFactory : ViewFactory[_]
  //initializes current controller
  def installCurrentController(useful_object : AnyRef) : Unit
  def blockView(block : Boolean = false) : Unit
  def getParent : Parent
}

class KinematicForwardBehavior extends GearViewBehavior{
  protected var currentGearViewController : GearGroupListViewController = _
  protected val currentGearViewControllerFactory : GearGroupListViewControllerFactory = new GearGroupListViewControllerFactory
  override def getCurrentController: GearParamsInput = currentGearViewController
  override def getCurrentFactory: ViewFactory[_] = currentGearViewControllerFactory
  override def installCurrentController(gearGroups : AnyRef): Unit = {
    try{
      val to_ret = currentGearViewControllerFactory
        .createView(gearGroups.asInstanceOf[List[GearGroup]])
        .asInstanceOf[GearGroupListViewController]
      currentGearViewController =  to_ret
    } catch {
      case e : Exception => e.printStackTrace()
    }
  }
  override def blockView(block: Boolean): Unit = {
    currentGearViewController.gearGroupListView.gearGroupList.setDisable(block)
  }
  override def getParent: Parent = {
    currentGearViewController.gearGroupListView.gearGroupList
  }
}