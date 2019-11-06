package planar_interface.view.GearGroupView

import java.net.URL

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Parent
import javafx.scene.control.{Label, SplitPane}
import javafx.scene.layout.AnchorPane
import planar_interface.view.GearListView.{GearListViewController, GearListViewControllerFactory}
import planar_interface.view.GearView.ViewFactory
import planar_structure.mechanism.GearGroup

class GearGroupFullView {
  @FXML
  var gearGroupAnchor : AnchorPane = _
  @FXML
  var gearGroupLabel : Label = _
  @FXML
  var gearGroupPane : SplitPane = _ //split pane where all info corresponding to the group is located
}

class GearGroupFullViewController(var gearGroupFullView: GearGroupFullView){
  //must get the common pane and fill it with gears of group on the left and common parameters for the group on the right
  //TODO understand what methods should be here
}

abstract class AbstractGearGroupFullViewControllerFactory(val location : String = "GearGroupFullView.fxml")
extends ViewFactory[AbstractGearGroupFullViewControllerFactory]{
  def getLocation : URL = {
    classOf[AbstractGearGroupFullViewControllerFactory].getResource(location)
  }
  var gearGroup : GearGroup = _
  def setGearGroup(gearGroup: GearGroup) : Unit = this.gearGroup = gearGroup
  val gearGroupOnlyViewControllerFactory : GearGroupOnlyViewControllerFactory = new GearGroupOnlyViewControllerFactory
  val gearListViewControllerFactory : GearListViewControllerFactory = new GearListViewControllerFactory
}

class GearGroupFullViewControllerFactory extends AbstractGearGroupFullViewControllerFactory{

  override def createView(): AnyRef = {
    //обновляем загрузчик для сохранения текущего контрллера и вида
    super.createView()
    //текущий контроллер
    val curr_controller = controller.asInstanceOf[GearGroupFullView]
    gearListViewControllerFactory.setGearsList(gearGroup.gear_list)
    gearGroupOnlyViewControllerFactory.setGearGroup(gearGroup)
    //теперь туда надо забросить то что мы полуаем от других фабрик
    val left = gearListViewControllerFactory.createView().asInstanceOf[GearListViewController]
    val right = gearGroupOnlyViewControllerFactory.createView().asInstanceOf[GearGroupOnlyViewController]
    curr_controller.gearGroupPane.getItems.add(left.gearListView.gearsList)
    curr_controller.gearGroupPane.getItems.add(right.gearGroupOnlyView.gearGroupOnlyPane)
    //создаем новую штуку
    new GearGroupFullViewController(curr_controller)
  }
}

/*
class GearGroupFullViewControllerFactory(gearGroup: GearGroup) extends AbstractGearGroupFullViewControllerFactory(gearGroup){
  // Отображаем сцену, содержащую корневой макет.
  override def createGearView(): GearGroupOnlyViewController= {
    val controller = loader.getController[GearGroupFullView]
    new GearGroupFullViewController(gearGroup, controller)
  }
}*/
