package planar_interface.view.KinematicSynthesisInputView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.layout.{GridPane, VBox}
import planar_interface.view.GearParamsInput
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.GearWheel

class GearListView {
@FXML
  var gearsList : VBox = _
}

class GearListViewController(var gearListView: GearListView, var gearsList : List[GearViewController]) extends GearParamsInput{
  init()
  def init() : Unit = {
    gearsList.foreach( controller => {
      val view: GridPane = controller.gearView.gearGridPane
      gearListView.gearsList.getChildren.add(view)
    }
    )
  }
  override def checkInput: Boolean = {
    gearsList.foldLeft(true)((left, right) => left & right.checkInput)
  }
  override def performSideEffect(): Unit = {
    gearsList.foreach(_.performSideEffect())
  }

  override def getParent: Parent = gearListView.gearsList

  override def getUsefulObject: AnyRef = {
    gearsList.map(_.getUsefulObject.asInstanceOf[GearViewWheelParams])
  }
  override def clearInput(): Unit = {
    gearsList.foreach(_.clearInput())
  }
}


class GearListViewControllerFactory(override val location : String = "GearListView.fxml")
  extends ViewFactory [GearListViewControllerFactory]{
  // Отображаем сцену, содержащую корневой макет.
  val gearViewControllerFactory : GearViewControllerFactory = new GearViewControllerFactory
  var gearsNumber : Short = _
  def setGearsNumber(some : Short): Unit = gearsNumber = some
  override def getLocation: URL = classOf[GearListViewControllerFactory].getResource(location)
  override def createView(): AnyRef= {
    super.createView() //updating loader
    //теперь её надо заполнить
    val gearControllersList = Range(0,gearsNumber).map((gearWheel) =>  {
      //устанавливаем на поток фабрики текущее колесо
      //gearViewFactory.gearWheel = gearWheel
      //получаем Pane под это колесо и записываем в Parent нашего контроллера
      gearViewControllerFactory.setGearNumber(gearWheel.toShort)
      val lower_controller = gearViewControllerFactory.createView()
      lower_controller
      //lower_controller.gearView.gearNumberLabel.setText(s"Колесо ${gearWheel._2 + 1}")
      //получаем контроллер
     // gearViewFactory.createGearView()
    })
    new GearListViewController(controller.asInstanceOf[GearListView], gearControllersList.toList)
  }
}