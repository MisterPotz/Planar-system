package planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupListView

import java.net.URL

import javafx.fxml.FXML
import javafx.scene.Parent
import javafx.scene.control.TextField
import javafx.scene.layout.VBox
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearGroupView.{AbstractGearGroupFullViewControllerFactory, GearGroupFullViewController, GearGroupFullViewControllerFactory}
import planar_interface.view.{GearParamsInput, TextCallbackChecker, TextCallbackCheckerSimple}
import planar_interface.view.mode_dependent_screen.kinematic_analysis.ArgumentServer
import planar_interface.view.mode_dependent_screen.kinematic_analysis.GearView.ViewFactory
import planar_structure.mechanism.GearGroup

class GearGroupListView {
  @FXML
  var root : VBox = _
  @FXML
  var satellitesTextField : TextField = _
@FXML
  var gearGroupList : VBox = _
}

class GearGroupListViewController(var gearGroupListView: GearGroupListView,
                                  var gearGroups : List[GearGroup],
                                  var controllers : List[GearGroupFullViewController])
extends GearParamsInput{
  init()

  val satellitesChecker : TextCallbackChecker = TextCallbackCheckerSimple((a) => a.toInt > 0 && a.toInt <= 9,
    () => gearGroupListView.satellitesTextField.getText(),
    (s) => gearGroupListView.satellitesTextField.setText(s),
    "Число вышло за допустимые пределы",
    "Введено не число", true, () => gearGroupListView.satellitesTextField.getPromptText)

  def init() : Unit = {
    controllers.zipWithIndex.foreach((controller) => {
      val view : Parent = controller._1.gearGroupFullView.gearGroupAnchor
      gearGroupListView.gearGroupList.getChildren.add(view)
      controller._1.gearGroupFullView.gearGroupLabel.setText(s"Группа зацеплений ${controller._2+1}")
    })
  }

  /**
   *
   * @param server место куда складываются аргументы подпрограмме анализа
   * @return возвращает {@code true} если аргумент записан успешно
   */
  def fillArguments(server : ArgumentServer) : Boolean = {
    if (checkInput){
      controllers.zipWithIndex.foreach(controller => {
        //сначала заполняем инфу из списка для колес, чтобы получить попутно размеры списка, не должны зависеть от типа механизма
        //напрямую
        val wheelListSize = controller._1.gearListViewController.gearsList.length
        controller._1.gearListViewController.gearsList.zipWithIndex.map{wheelCon => {
          server.x.addOne( controller._2
            * wheelListSize + wheelCon._2 -> wheelCon._1.xChecker.getText().toDouble)

        }}
        controller._1.gearListViewController.gearsList.zipWithIndex.map{wheelCon => {
          server.z.addOne( controller._2
            * wheelListSize + wheelCon._2 -> wheelCon._1.zChecker.getText().toInt)

        }}
        controller._1.gearListViewController.gearsList.zipWithIndex.map{wheelCon => {
          server.ha.addOne( controller._2
            * wheelListSize + wheelCon._2 -> wheelCon._1.haChecker.getText().toDouble)

        }}
        controller._1.gearListViewController.gearsList.zipWithIndex.map{wheelCon => {
          server.c.addOne( controller._2
            * wheelListSize + wheelCon._2 -> wheelCon._1.caChecker.getText().toDouble)

        }}
        Range(0, wheelListSize).foreach( index =>
          server.m.addOne(controller._2 * wheelListSize + index ->
            controller._1.gearGroupOnlyViewController.mChecker.getText().toDouble))
        Range(0, wheelListSize).foreach( index =>
          server.beta.addOne(controller._2 * wheelListSize + index ->
            controller._1.gearGroupOnlyViewController.betaChecker.getText().toDouble))
        Range(0, wheelListSize).foreach( index =>
          server.alpha.addOne(controller._2 * wheelListSize + index ->
            controller._1.gearGroupOnlyViewController.alphaChecker.getText().toDouble))
      })
      server.satellites = satellitesChecker.getText().toInt
      true
    } else false
  }

  def deactivateViews(deactivate : Boolean) = {
    gearGroupListView.root.setDisable(deactivate)
  }
  def enter() : Unit = {
    //must check all the fields for the good input
  }

  override def blockView: Unit = {
    getParent.setDisable(true)
  }
  override def isBlocked: Boolean = {
    getParent.isDisabled
  }
  override def unblockView: Unit = {
    getParent.setDisable(false)
  }

  override def checkInput: Boolean = {
    controllers.foldLeft(true)((x,y ) => x & y.checkInput) & satellitesChecker.checkIfElseSet()
  }

  override def performSideEffect(): Unit = {
    //controllers.foreach(x => x.performSideEffect())
  }

  override def getParent: Parent = gearGroupListView.root

  override def clearInput(): Unit = {
    controllers.foreach(_.clearInput())
  }
}

abstract class AbstractGearGroupListViewControllerFactory(override val location: String = "GearGroupListView.fxml")
  extends ViewFactory[AbstractGearGroupListViewControllerFactory]{
  override def getLocation: URL = classOf[AbstractGearGroupListViewControllerFactory].getResource(location)
  val gearGroupFullViewControllerFactory : AbstractGearGroupFullViewControllerFactory =
    new GearGroupFullViewControllerFactory
  def createView(gearGroups : List[GearGroup]) : AnyRef
  override def createView(): AnyRef = super.createView()
  var gearGroups : List[GearGroup] = _
  def setGearGroups(gearGroups : List[GearGroup]): Unit = this.gearGroups = gearGroups
}

class GearGroupListViewControllerFactory extends AbstractGearGroupListViewControllerFactory{
  override def createView(): AnyRef = {
    super.createView()
    val controller : GearGroupListView = this.controller.asInstanceOf[GearGroupListView]
    val gearGroupViewControllers = gearGroups.zipWithIndex.map( (gearGroup) =>{
      gearGroupFullViewControllerFactory.setGearGroup(gearGroup._1)
      gearGroupFullViewControllerFactory.createView().asInstanceOf[GearGroupFullViewController]
    }
    )
    new GearGroupListViewController(controller, gearGroups, gearGroupViewControllers )
  }
  override def createView(gearGroups : List[GearGroup]): AnyRef = {
    this.gearGroups = gearGroups
    createView()
  }
}