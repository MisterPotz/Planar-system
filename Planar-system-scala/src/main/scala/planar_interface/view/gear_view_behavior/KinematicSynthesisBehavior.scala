package planar_interface.view.gear_view_behavior

import javafx.scene.Parent
import planar_interface.view.GearGroupListView.{GearGroupListViewController, GearGroupListViewControllerFactory}
import planar_interface.view.GearParamsInput
import planar_interface.view.GearView.ViewFactory
import planar_interface.view.KinematicSynthesisInputView.{AbstractKSIVC_Factory, KSIVC_Factory, KinematicSynthesisInputViewController}
import planar_structure.mechanism.GearGroup
import planar_structure.mechanism.types.MechanismType

class KinematicSynthesisBehavior extends GearViewBehavior{
  protected var currentGearViewController : KinematicSynthesisInputViewController = _
  protected val currentGearViewControllerFactory : AbstractKSIVC_Factory = new KSIVC_Factory
  override def getCurrentController: GearParamsInput = currentGearViewController
  override def getCurrentFactory: ViewFactory[_] = currentGearViewControllerFactory
  override def installCurrentController(some : AnyRef): Unit = {
    try{
      currentGearViewControllerFactory.setMechanismType(some.asInstanceOf[MechanismType])
      val to_ret = currentGearViewControllerFactory
        .createView()
        .asInstanceOf[KinematicSynthesisInputViewController]
      currentGearViewController =  to_ret
    } catch {
      case e : Exception => e.printStackTrace()
    }
  }
  override def blockView(block: Boolean): Unit = {
    currentGearViewController.getParent.setDisable(block)
  }
  override def getParent: Parent = {
    currentGearViewController.getParent
  }
}
