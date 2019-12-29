package planar_interface.view.result

import javafx.scene.Node
import planar_interface.Observer
import planar_structure.mechanism.types.ProcessType

trait AbstractResultViewControllerFactory {
  def createResultViewController(s : String) : Option[AbstractResultViewController]
}

trait AbstractResultViewController extends Observer {
  def getNode : Node
  def showLoading(boolean: Boolean)
  def clearResults() : Unit
 // def tellNoCalculations(boolean: Boolean)
}

object ResultViewControllerFactory extends AbstractResultViewControllerFactory{
  val kinematicForwardViewControllerFactory : AbstractKinematicForwardViewControllerFactory =
     KinematicForwardViewControllerFactory
  override def createResultViewController(s: String): Option[AbstractResultViewController] = {
    s match {
        //TODO add synthesis
      case ProcessType.KINEMATIC_ANALYSIS_FORWARD =>
        Some(kinematicForwardViewControllerFactory.createView().asInstanceOf[AbstractResultViewController])
      case _ => None
    }
  }
}

object ResultViewCode{
  val KINEMATIC_FORWARD : String = "kinematic forward"
  val KINEMATIC_SYNTHESIS : String = "kinematic synthesis"
  //...and so on
}
