package planar_interface

import java.awt.event.{ActionEvent, ActionListener}

import javax.swing.{JComponent, JDesktopPane, JPanel}
import planar_structure.mechanism.GearGroup

abstract class GearGroupParameter (val gear_group : GearGroup) extends JPanel with ActionListener{
  val lists : List[JComponent] //list of JPanels with inputs for each gear
  //setting for this geargroupparameter JComponents
  override def actionPerformed(actionEvent: ActionEvent): Unit = ???
}
