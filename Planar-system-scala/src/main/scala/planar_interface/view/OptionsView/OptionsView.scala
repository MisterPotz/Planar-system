package planar_interface.view.OptionsView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.control.{Button, MenuBar}
import javafx.scene.layout.{AnchorPane, BorderPane}
import planar_interface.view.GearGroupView.AbstractGearGroupOnlyViewControllerFactory
import planar_interface.view.GearView.ViewFactory

class OptionsView {
  @FXML
  var appBorderPane : BorderPane = _
  @FXML
  var eraseButton : Button = _
  @FXML
  var enterButton : Button = _
  @FXML
  var settingsAnchorPane : AnchorPane = _
  @FXML
  var resultsAnchorPane : AnchorPane = _
  @FXML
  var appMenuBar : MenuBar =  _
}
class OptionsViewController(var optionsView: OptionsView){

}
abstract class AbstractOptionsViewControllerFactory(val location : String = "OptionsView.fxml") extends ViewFactory[AbstractOptionsViewControllerFactory]{
  override def getLocation : URL = {
    classOf[AbstractOptionsViewControllerFactory].getResource(location)
  }
}

class OptionsViewControllerFactory extends AbstractOptionsViewControllerFactory {
  override def createView(): AnyRef = {
    super.createView() //updating parent and controller
    val controller = this.controller.asInstanceOf[OptionsView]
    new OptionsViewController(controller)
  }
}
