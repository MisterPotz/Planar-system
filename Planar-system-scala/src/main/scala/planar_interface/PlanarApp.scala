package planar_interface
import java.awt.event.{ActionEvent, ActionListener}

import javafx.geometry.Insets
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.{Border, FlowPane, Pane}
import javafx.scene.{Node, Parent}
import planar_structure.mechanism.GearWheel
import javafx.animation.Animation
import javafx.animation.FadeTransition
import javafx.util.Duration

//разбить вложенную рут ноду и вложенные на несколько классов
abstract  class GearParameter(val gearWheel : GearWheel, val txt : String) extends FlowPane with ActionListener{
  setHgap(10)
  setPadding(new Insets(10))
  val txt_Input = new TextField("")
  txt_Input.setMaxWidth(100)
  val btn_Input = new Button("Enter")
  val lbl_Name = new Label(txt)
  getChildren.addAll(lbl_Name, txt_Input, btn_Input)
  protected def showRight(boolean: Boolean): Unit = {
   // val s : Pseudo
  }
  protected def showWrong(boolean: Boolean) : Unit = {
  //  fadeTransition.playFromStart()
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


//gear agent interconnects interface components for buttonsof interface and gears themselves
/*
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

}*/
