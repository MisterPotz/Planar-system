package planar_interface.view.GearListView

import java.net.URL

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Parent
import javafx.scene.control.Button
import javafx.scene.layout.{GridPane, VBox}
import planar_interface.view.GearView.{AbstractGearViewControllerFactory, GearView, GearViewController, GearViewControllerFactory, ViewFactory}
import planar_interface.view.GearGroupView.{AbstractGearGroupOnlyViewControllerFactory, GearGroupOnlyViewController}
import planar_structure.mechanism.GearWheel

class GearListView {
@FXML
  var gearsList : VBox = _
}

class GearListViewController(/*var gearsList : List[GearViewController], */var gearListView: GearListView) {

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
    val gearControllersList = gearsList.zipWithIndex.foreach((gearWheel) =>  {
      //устанавливаем на поток фабрики текущее колесо
      //gearViewFactory.gearWheel = gearWheel
      //получаем Pane под это колесо и записываем в Parent нашего контроллера
      gearViewControllerFactory.setGearWheel(gearWheel._1)
      val lower_controller = gearViewControllerFactory.createView().asInstanceOf[GearViewController]
      lower_controller.a =s"${gearWheel._1.holder.z}"
      val view : GridPane = lower_controller.gearView.gearGridPane
      controller.asInstanceOf[GearListView].gearsList.getChildren.add(view)
      lower_controller.gearView.gearNumberLabel.setText(s"Колесо ${gearWheel._2 + 1}")
      //получаем контроллер
     // gearViewFactory.createGearView()
    })
    new GearListViewController(/*gearControllersList, */controller.asInstanceOf[GearListView])
  }
}