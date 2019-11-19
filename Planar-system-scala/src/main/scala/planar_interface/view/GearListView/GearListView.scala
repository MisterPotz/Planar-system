package planar_interface.view.GearListView

import java.net.URL

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Parent
import javafx.scene.control.Button
import javafx.scene.layout.{GridPane, VBox}
import planar_interface.view.GearView.{AbstractGearViewControllerFactory, GearView, GearViewController, GearViewControllerFactory, ViewFactory}
import planar_interface.view.GearGroupView.{AbstractGearGroupOnlyViewControllerFactory, GearGroupOnlyViewController}
import planar_interface.view.GearParamsInput
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

  override def clearInput(): Unit = {
    gearsList.foreach(_.clearInput())
  }
}

abstract class AbstractGearListViewControllerFactory(override val location : String = "GearListView.fxml")
extends ViewFactory[AbstractGearListViewControllerFactory]{
  val gearViewControllerFactory : AbstractGearViewControllerFactory
  var gearsList : List[GearWheel] = _
  def setGearsList(gearsList : List[GearWheel]) : Unit = this.gearsList = gearsList
  override def getLocation: URL = classOf[AbstractGearListViewControllerFactory].getResource(location)
}

class GearListViewControllerFactory extends AbstractGearListViewControllerFactory{
  // Отображаем сцену, содержащую корневой макет.
  override val gearViewControllerFactory : AbstractGearViewControllerFactory = new GearViewControllerFactory
  override def createView(): AnyRef= {
    super.createView() //updating loader
    //теперь её надо заполнить
    val gearControllersList = gearsList.zipWithIndex.map((gearWheel) =>  {
      //устанавливаем на поток фабрики текущее колесо
      //gearViewFactory.gearWheel = gearWheel
      //получаем Pane под это колесо и записываем в Parent нашего контроллера
      gearViewControllerFactory.setGearWheel(gearWheel._1)
      val lower_controller = gearViewControllerFactory.createView().asInstanceOf[GearViewController]
      lower_controller
      //lower_controller.gearView.gearNumberLabel.setText(s"Колесо ${gearWheel._2 + 1}")
      //получаем контроллер
     // gearViewFactory.createGearView()
    })
    new GearListViewController(controller.asInstanceOf[GearListView], gearControllersList)
  }
}