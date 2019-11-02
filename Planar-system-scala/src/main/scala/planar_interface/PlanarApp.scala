package planar_interface
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, FlowLayout, GridLayout}

import javax.swing._
import planar_structure.mechanism.GearWheel

import scala.swing.Dimension
trait SwingEnums {
  implicit def actionToInt(a : SFrame.Action) : Int = {
    SFrame(a)
  }
}
object SFrame{
  sealed trait Action
  case object EXIT_ON_CLOSE extends Action
  def apply(a : Action) : Int = {
    a match {
      case EXIT_ON_CLOSE => 3
    }
  }
}

//gear agent interconnects interface components for buttonsof interface and gears themselves
abstract class GearParameter(val gearWheel : GearWheel, val txt : String) extends JPanel with ActionListener{
  val txt_Input = new JTextField("")
  txt_Input.setActionCommand("entered")
  txt_Input.addActionListener(this)
  txt_Input.setHorizontalAlignment(0)
  txt_Input.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 2, true))
  val btn_Input = new JButton("Enter")
  btn_Input.setActionCommand("entered")
  btn_Input.addActionListener(this)
  val lbl_Name = new JLabel(txt)
  lbl_Name.setHorizontalTextPosition(2)
  val lbl_Curr = new JLabel(txt)
  val b = new GroupLayout(this)
  val t = new Timer(500, this)
  t.setActionCommand("timer")
  val t_green = new Timer(500, this)
  t_green.setActionCommand("green-signal")
  val separator = new JSeparator(1)
  setLayout(b)
  b.setAutoCreateContainerGaps(true)
  b.setAutoCreateGaps(true)
  b.setHorizontalGroup(
      b.createSequentialGroup()
        .addComponent(lbl_Name, GroupLayout.PREFERRED_SIZE, 200, GroupLayout.PREFERRED_SIZE)
        .addComponent(txt_Input, GroupLayout.PREFERRED_SIZE, 70,GroupLayout.PREFERRED_SIZE)
      .addComponent(separator)
        .addComponent(btn_Input, GroupLayout.PREFERRED_SIZE, 90, GroupLayout.PREFERRED_SIZE)
      //.addComponent(lbl_Wrong)
  )
  b.setVerticalGroup(
    b.createParallelGroup(GroupLayout.Alignment.BASELINE)
        .addComponent(lbl_Name, GroupLayout.PREFERRED_SIZE, 30, GroupLayout.PREFERRED_SIZE)
        .addComponent(txt_Input, GroupLayout.PREFERRED_SIZE, 30, GroupLayout.PREFERRED_SIZE)
    .addComponent(separator)
        .addComponent(btn_Input, GroupLayout.PREFERRED_SIZE, 30, GroupLayout.PREFERRED_SIZE)
   //   .addComponent(lbl_Wrong)
  )
  val a = new GridLayout(1,3,10,5)
  val dimension = new Dimension(380, 40)
  setMaximumSize(dimension)
  setMinimumSize(dimension)
  protected def showRight(boolean: Boolean): Unit = {
    txt_Input.setBorder(if (boolean) BorderFactory.createLineBorder(Color.GREEN, 5, true) else (BorderFactory.createLineBorder(Color.LIGHT_GRAY, 2, true)))
    t_green.restart()
  }
  protected def showWrong(boolean: Boolean) : Unit = {
    txt_Input.setBorder(if (boolean) BorderFactory.createLineBorder(Color.RED, 5, true) else (BorderFactory.createLineBorder(Color.LIGHT_GRAY, 2, true)))
    t.restart()
  }
  protected def checkText(a : String) : Boolean
  protected def updateGear(a : String) : Unit
  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    actionEvent.getActionCommand match {
      case "entered" => if (checkText(txt_Input.getText())){
        updateGear(txt_Input.getText)
        showRight(true)
      } else showWrong(true)
      case "timer" => {showWrong(false);}
      case "green-signal" => showRight(false)
    }
  }
}

class GearParameterZ(gearWheel: GearWheel) extends GearParameter(gearWheel = gearWheel, "Число зубьев z"){
  override protected def checkText(a: String): Boolean = {
    try {
      if (a.toInt > 0 && a.toInt < 1000) true else false
    }
    catch {
      case _ : Exception =>  false
    }
  }
  override protected def updateGear(a: String): Unit ={
    gearWheel.holder.z = a.toInt
  }
}
class GearParameterX(gearWheel : GearWheel) extends GearParameter(gearWheel = gearWheel, "Смещение инструмента x"){
  override protected def checkText(a: String): Boolean = {
    try{
      if (a.toDouble > -10 && a.toDouble < 10) true else false
    } catch {
      case _ : Exception => false
    }
  }
  override protected def updateGear(a: String): Unit = gearWheel.holder.x = a.toDouble
}

//GearParameters incapsulates GearGroups and GearGroup incapsulates concrete settings for textfields for gear parameters

class PlanarApp extends JFrame{

}