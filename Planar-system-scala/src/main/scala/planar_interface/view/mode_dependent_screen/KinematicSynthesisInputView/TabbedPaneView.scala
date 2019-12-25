package planar_interface.view.mode_dependent_screen.KinematicSynthesisInputView

import java.net.URL

import javafx.scene.control.TitledPane
import javafx.fxml.FXML
import javafx.geometry.Insets
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.layout.{GridPane, VBox}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.report.AdditionalInfoSynthesized

import scala.collection.mutable.ListBuffer

object TitledPaneViewControllerW {
  val gridMap: Map[String, Int] = Map(
    "название" -> 0,
    "Z" -> 1,
    "M" -> 2,
    "X" -> 3,
    "β" -> 4,
    "α" -> 5,
    "Материал" -> 6)
  val name = "Колесо "
}

trait HelpDouble {

  implicit class DoubleW(double: Double) {
    def toStringFormatted: String = {
      toStringFormatted(2)
    }

    def toStringFormatted(digits : Int) : String = {
      val  rounded = math.round(double * math.pow(10, digits)) / math.pow(10, digits)
      val roundedString = rounded.toString
      if (rounded == 0){
        "0"
      } else
      {
        var pre = roundedString.slice(0, roundedString.indexOf(".")+digits+1)
        if (pre.indexOf(".") < pre.indexOf("0")){
          while (pre.indexOf(".") < pre.lastIndexOf("0")){
            pre = roundedString.slice(0, pre.lastIndexOf("0"))
            if (pre.indexOf(".") +1 == pre.length){
              pre = roundedString.slice(0, pre.length- 1)
            }
          }
        }
        pre
      }
    }
  }

}


class TitledPaneViewController {
  @FXML
  var zGridPane: GridPane = _
  @FXML
  var root: VBox = _
  @FXML
  var otherGridPane: GridPane = _
  @FXML
  var resULabel: Label = _
  @FXML
  var epsULabel: Label = _
  @FXML
  var aw2Label: Label = _
  @FXML
  var upsAw2Label: Label = _
  @FXML
  var DSigFLabel: Label = _
  @FXML
  var DSigHLabel: Label = _
  @FXML
  var maxSizeLabel : Label = _

}


class TitledPaneViewControllerW(var titledPaneViewController: TitledPaneViewController) extends HelpDouble {
  def con: TitledPaneViewController = titledPaneViewController

  lazy val parent: TitledPane = new TitledPane()

  def setAll(string: String): Unit = {
    setUpsAw2(string)
    setAw2(string)
    setResU(string)
    setEpsU(string)
    setDSigH(string)
    setDSigF(string)
  }

  def setTitle(string: String): Unit = {
    parent.setText(string)
  }

  case class ZRow(name: String, z: String, x: String, m: String, beta: String, alpha: String, material: String)

  def getZRow(zRow: ZRow): List[Node] = {
    List(
      (getIndexOfParameter("название"), zRow.name),
      (getIndexOfParameter("X"), zRow.x),
      (getIndexOfParameter("M"), zRow.m),
      (getIndexOfParameter("α"), zRow.alpha),
      (getIndexOfParameter("β"), zRow.beta),
      (getIndexOfParameter("Материал"), zRow.material),
      (getIndexOfParameter("Z"), zRow.z)
    ).sortBy(_._1).map(str => {
      val label = new Label(str._2)
      label.setPadding(new Insets(4,4,4,4))
      label
    })
  }

  def getIndexOfParameter(str: String): Int = {
    TitledPaneViewControllerW.gridMap(str)
  }

  def setWheels(mechanism: Mechanism, wheelIndeces: List[String]): Unit = {
    val gridPane = con.zGridPane
    val initial = 1
    val z = mechanism.getGears.map(_.holder.z)
    val m = mechanism.getGears.map(_.holder.m)
    val x = mechanism.getGears.map(_.holder.x)
    val alpha = mechanism.getGears.map(_.holder.alpha)
    val beta = mechanism.getGears.map(_.holder.beta)
    val material = mechanism.getGears.map(_.material_holder.materialTableRow.StMark(0))
    wheelIndeces.zipWithIndex.foreach(index => {
      val row =getZRow(ZRow(name = TitledPaneViewControllerW.name + index._1,
        z(index._2).toString,
        x(index._2).toStringFormatted,
        m(index._2).toStringFormatted,
        beta(index._2).toDegrees.toStringFormatted,
        alpha(index._2).toDegrees.toStringFormatted,
        material(index._2),
      ))
      row.zipWithIndex.foreach(node =>
        gridPane.add(node._1,node._2, index._2+initial))
    })
  }

  def setMechanism(index: Int,
                   mech: Mechanism,
                   wheelIndeces: List[String],
                   additionalInfo: AdditionalInfoSynthesized,
                   targetU: Double): Unit = {
    val resu = additionalInfo.resU.toStringFormatted
    val epsu = additionalInfo.uAccuracy.toStringFormatted + " %"
    val aw2 = additionalInfo.aw2.toStringFormatted + " мм"
    val epsAw = additionalInfo.alignmentAccuracy.toStringFormatted + " %"
    val dsigh = additionalInfo.allowedTension.sigma_h.toStringFormatted + " МПа"
    val dsigf = additionalInfo.allowedTension.sigma_f.toStringFormatted + " МПа"
    val size = additionalInfo.maximumSize.toStringFormatted + " мм"
    setResU(resu)
    setEpsU(epsu)
    setAw2(aw2)
    setUpsAw2(epsAw)
    setDSigF(dsigf)
    setDSigH(dsigh)
    setWheels(mech,wheelIndeces)
    setSize(size)
    parent.setContent(con.root)
    setTitle(index.toString)
  }

  def setUpsAw2(str: String): Unit = {
    con.upsAw2Label.setText(str)
  }

  def setAw2(str: String): Unit = {
    con.aw2Label.setText(str)
  }

  def setResU(str: String): Unit = {
    con.resULabel.setText(str)
  }

  def setEpsU(str: String): Unit = {
    con.epsULabel.setText(str)
  }

  def setDSigH(str: String): Unit = {
    con.DSigHLabel.setText(str)
  }

  def setDSigF(str: String): Unit = {
    con.DSigFLabel.setText(str)
  }
  def setSize(str : String) : Unit = {
    con.maxSizeLabel.setText(str)
  }
}


object TitledPaneViewControllerWFactory
  extends ViewFactory[TitledPaneViewControllerW] {

  override val location: String = "TabbedPaneView.fxml"

  override def getLocation: URL = TitledPaneViewControllerWFactory.getClass.getResource(location)

  override def createView(): AnyRef = {
    super.createView()
    new TitledPaneViewControllerW(controller.asInstanceOf[TitledPaneViewController])
  }
}