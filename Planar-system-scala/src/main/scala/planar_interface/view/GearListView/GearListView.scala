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

abstract class AbstractGearListViewControllerFactory(var gearsList : List[GearWheel],
                                                     override val location : String = "GearListView.fxml")
extends ViewFactory[AbstractGearListViewControllerFactory]{
  val gearViewControllerFactory : AbstractGearViewControllerFactory

  override def getLocation: URL = classOf[AbstractGearListViewControllerFactory].getResource(location)
}

class GearListViewControllerFactory(gearsList : List[GearWheel]) extends AbstractGearListViewControllerFactory(gearsList){
  // Отображаем сцену, содержащую корневой макет.
  override val gearViewControllerFactory : AbstractGearViewControllerFactory = new GearViewControllerFactory(gearsList(0))
  override def createView(): AnyRef= {
    super.createView() //updating loader
    //теперь её надо заполнить
    val gearControllersList = gearsList.map((gearWheel) =>  {
      //устанавливаем на поток фабрики текущее колесо
      //gearViewFactory.gearWheel = gearWheel
      //получаем Pane под это колесо и записываем в Parent нашего контроллера
      val lower_controller = gearViewControllerFactory.createView()
      lower_controller.asInstanceOf[GearViewController].a =s"${gearWheel.holder.z}"
      val view : GridPane = lower_controller.asInstanceOf[GearViewController].gearView.gearGridPane
      controller.asInstanceOf[GearListView].gearsList.getChildren.add(view)
      //получаем контроллер
     // gearViewFactory.createGearView()
    })
    new GearListViewController(/*gearControllersList, */controller.asInstanceOf[GearListView])
  }
}