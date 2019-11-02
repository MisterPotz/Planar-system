package planar_structure
import javax.swing._
import java.awt._
import java.awt.event._

import planar_interface.{GearParameter, GearParameterX, GearParameterZ, GearParametersJ}
import planar_structure.mechanism.Mechanism2KH

import scala.swing._
import scala.util.Random
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

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTextArea
import javax.swing.JTextField
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.event.ActionListener

object Main extends SwingEnums {
  val newline = "\n"

  /**
   * Create the GUI and show it.  For thread safety,
   * this method should be invoked from the
   * event dispatch thread.
   */
  private def createAndShowGUI(): Unit = { //Create and set up the window.
    val frame = new JFrame("TextDemo")
    frame.setDefaultCloseOperation(SFrame.EXIT_ON_CLOSE)
    //Add contents to the window.
    frame.add(new Demo().init())
    //Display the window.
    frame.pack()
    frame.setVisible(true)
  }

  def main(args: Array[String]): Unit = { //Schedule a job for the event dispatch thread:
    //creating and showing this application's GUI.
  /*  javax.swing.SwingUtilities.invokeLater(new Runnable() {
      override def run(): Unit = {createAndShowGUI()}})*/
  /*  val mech = Mechanism2KH("InternalInternal_CarrierInput")
    val gears = mech.characteristics.gearStructureCharacteristic.getGearList
    gears(0).holder.z = 87
    gears(1).holder.z = 34
    gears(2).holder.z = 28
    gears(3).holder.z = 81
    println(s"${mech.methods.geometricMethods.getGearRatio}") //TODO DANGEROUS BUG with all gears being interconnected with prepareAllGears functions. Must be overloaded in classes
    mech.characteristics.gearStructureCharacteristic.getGearGroup(0)
    println(s"${mech.methods.geometricMethods.noPruningOnAll}")
    println(s"${mech.methods.geometricMethods.neighborhoodCondition}")
    println(s"${mech.methods.geometricMethods.assemblyCondition}")
    println(s"${mech.methods.geometricMethods.alignmentCondition}")
    println(s"${mech.methods.geometricMethods.interferenceCondition(0)}")
    println(s"${mech.methods.geometricMethods.interferenceCondition(1)}")*/
    val mech = Mechanism2KH("InternalInternal_CarrierInput")
    val gear = mech.characteristics.gearStructureCharacteristic.getGear(0)
    SwingUtilities.invokeLater(() => {
    /*  val curr_inf = new PaintPanelDemo
      curr_inf.setVisible(true)*/
      val frame = new JFrame("Some title")
      frame.setLocationRelativeTo(null)
      frame.setDefaultCloseOperation(SFrame.EXIT_ON_CLOSE)
      frame.setLayout(new FlowLayout())
      frame.add(new GearParameterZ(gear))
      frame.add(new GearParameterX(gear))
      frame.setPreferredSize(new Dimension(400, 250))
      frame.setMinimumSize(frame.getPreferredSize)
        //frame.add(new GearParameters())
      frame.setVisible(true)
    })
   // EventQueue.invokeLater( () => {var ex = new SimpleEx(); ex.setVisible(true) })
  }
}

class Demo extends JPanel(new GridBagLayout) with ActionListener {
  def init(): Demo = {
    textArea = new JTextArea(5, 20)
    textField = new JTextField(20)
    textField.addActionListener(this)
    textArea.setEditable(false)
    val scrollPane = new JScrollPane(textArea)
    //Add Components to this panel.
    val c = new GridBagConstraints
    c.gridwidth = GridBagConstraints.REMAINDER
    c.fill = GridBagConstraints.HORIZONTAL
    add(textField, c)
    c.fill = GridBagConstraints.BOTH
    c.weightx = 1.0
    c.weighty = 1.0
    add(scrollPane, c)
    this}

  override def actionPerformed(evt: ActionEvent): Unit = {
    val text = textField.getText
    textArea.append(text + Main.newline)
    textField.selectAll()
    //Make sure the new text is visible, even if there
    //was a selection in the text area.
    textArea.setCaretPosition(textArea.getDocument.getLength)
  }
  protected var textField : JTextField = null
  protected var textArea  : JTextArea = null

 // override def actionPerformed(actionEvent: ActionEvent): Unit = ???
}

class SimpleEx extends JFrame with SwingEnums {
  setTitle("Simple example")
  setSize(300, 200)
  setLocationRelativeTo(null)
  setDefaultCloseOperation(SFrame.EXIT_ON_CLOSE)
}
class QuitButtonEx extends JFrame with SwingEnums{
  var quitButton = new JButton("Quit")
  quitButton.addActionListener((event) => System.exit(0))
  quitButton.setToolTipText("Quit button for quitting")
  var lbl = new JLabel("Some label")
  lbl.setToolTipText("Some lab")
  createLayout(quitButton, lbl)
  setTitle("Quit Button")
  setSize(300, 200)
  setLocationRelativeTo(null)
  setDefaultCloseOperation(SFrame.EXIT_ON_CLOSE)

  import javax.swing.GroupLayout
  import javax.swing.JComponent

  private def createLayout(arg: JComponent*): Unit = {
    val pane = getContentPane
    val gl = new GroupLayout(pane)
    pane.setLayout(gl)
    gl.setAutoCreateContainerGaps(true)
    gl.setHorizontalGroup(gl.createParallelGroup().addComponent(arg(0)).addGap(120).addComponent(arg(1)))
    gl.setVerticalGroup(gl.createSequentialGroup.addComponent(arg(1)).addGap(20).addComponent(arg(0)))
    pack()
  }
}


class ButtonCounter extends JFrame with SwingEnums{
  ClickButton.setUpObj
  QuitButton.setUpObj
  ClickState.setUpObj
  createLayout( ClickButton, ClickState,  QuitButton)
  setTitle("Button Counter")
  setSize(300, 300)
  setLocationRelativeTo(null)
  setDefaultCloseOperation(SFrame.EXIT_ON_CLOSE)
  pack()
  def init() : ButtonCounter = {
    //ClickState.init()
    this
  }
  implicit def objSetupToComponent(a : ObjectSetup) : JComponent = a.compoment
  trait ObjectSetup {
    val compoment : JComponent
    def setUpObj: Unit = {}
  }
  object QuitButton extends ObjectSetup {
    override val compoment: JComponent = new JButton("Quit")
    override def setUpObj: Unit = {
      compoment.setToolTipText("Close program")
      compoment.asInstanceOf[JButton].addActionListener((_) => System.exit(0)) ;
      compoment.setSize(50, 20)
      super.setUpObj}
  }
  object ClickButton extends ObjectSetup {
    override val compoment: JComponent = new JButton("Click me bro")
    override def setUpObj: Unit = {
      compoment.asInstanceOf[JButton].addActionListener(ClickState)
      compoment.setSize(50, 20)
      super.setUpObj}
  }

  object ClickState extends ActionListener with ObjectSetup {
    override def setUpObj : Unit = {
      init()
      super.setUpObj
    }
    def init() : Unit = {
      compoment.asInstanceOf[JLabel]. setHorizontalAlignment(0)
      compoment.asInstanceOf[JLabel].setVerticalAlignment(0)
      displayClicks()
    }
    var counted_clicks : Int = 0
    def displayClicks() : Unit = {
      counted_clicks match {
        case 0 => compoment.asInstanceOf[JLabel].setText("No clicks made yet")
        case _ => compoment.asInstanceOf[JLabel].setText(s"You've made $counted_clicks")
      }
    }
    override def actionPerformed(actionEvent: ActionEvent): Unit = {
      counted_clicks += 1
      displayClicks()
    }
    override val compoment: JComponent = new JLabel("")
  }
  def createLayout(args : JComponent*) : Unit = {
    val pane = new JPanel
    pane.setLayout(new FlowLayout())
    add(pane)
    pane.add(args(0))
    pane.add(args(1))
    pane.add(args(2))
  }

}



class PaintPanelDemo extends JFrame with SwingEnums {
  setTitle("Paint Demo")
  setSize(200,150)
  setDefaultCloseOperation(SFrame.EXIT_ON_CLOSE)
  add(PaintPanel)
  object PaintPanel extends JPanel{
    setBorder(BorderFactory.createLineBorder(Color.RED, 5))
    setLayout(new BorderLayout())
    val img = new ImageIcon("some.png")
    val lbl : JLabel = new JLabel("This a circle yo")
    lbl.setIcon(img)
    lbl.setHorizontalAlignment(0)
    lbl.setVerticalAlignment(0)
    add(lbl, BorderLayout.CENTER)
    def init() : Unit = {

    }
    var insets_ : Insets = _
    var rand : Random = new Random()

    override protected def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      var r_percent : Double = 0.4
      val ins = getInsets()
      val height = getHeight
      val width = getWidth
      def min : Int = (if (height > width) width else height)
      var x : Int =  width / 2 + ((r_percent * min)).toInt;
      var y : Int =  height / 2 + (r_percent * min).toInt;
      var x2 : Int= 0;
      var y2 : Int = 0
      val range = for (i <- Range(1, 1000)) yield ((2 * math.Pi *  i) / 1000)
      for (i <- range){
        x2 = width / 2 + (r_percent * min * math.cos(i)).toInt
        y2 = height / 2 + (r_percent * min * math.sin(i)).toInt
        x = x2
        y = y2
        g.drawLine(x,y,x2,y2)
      }
    }
  }
  var jlab : JLabel = _

}