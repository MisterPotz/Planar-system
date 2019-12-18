package planar_interface.view.mode_dependent_screen

import javafx.scene.{Node, Parent}
import javafx.scene.control.SplitPane
import planar_interface.{Event, Observable, Observer}
import planar_interface.view.GearParamsInput

object InputResultPairViewInterface{
  trait Argument
  trait Result
  //request for setting views of subclasses controller in the higher order windows
  case class SetAsView(node : Node) extends Event

  case class SetResult(res : Option[Result]) extends Event

  /**
   * arguments we may get during runtime in fields, entered by user, may not
   * be of appropriate type and format. to prevent mistakes of such type
   * all arguments must be bundled inside Option. parent controller, in case of
   * None, will block important elements
   */
  //case class SendArguments(arguments : Option[Arguments]) extends Event
  case class RequestArguments(cb : (Option[Argument]) => Unit) extends Event
  case class RequestRootNode(cb : (Node) => Unit) extends Event


}

/**
 * интерфейс предназначенный для отображения view режима
 * подразумевается что на уровне этого интерфейса не происходит никаких событий
 * о которых должен знать главный контроллер
 * но должен быть предоставлен метод для установки результата
 * и должен быть метод для отправки результата
 */

trait InputResultPairViewInterface extends Observer with GearParamsInput{
  private val splitPane : SplitPane = new SplitPane()
  override def getParent: Parent = splitPane
  override def onChange(event: Event): Unit = {
    event match {
      case InputResultPairViewInterface.RequestArguments(cb) => cb(getArguments())
      case InputResultPairViewInterface.SetResult(res) => setResult(res)
      case InputResultPairViewInterface.RequestRootNode(cb) => cb(getRoot)
      case _ => ()
    }
  }
  def setContentView(left: Node, right: Node) : Unit = {
    splitPane.getItems.addAll(left, right)
  }
  /**
   *
   * result of any calculation must be inherited from Result trait
   */
  def setResult(result : Option[InputResultPairViewInterface.Result]) : Unit

  /**
   *
   *  must be inherited from Argument trait, represents an input in a program
   */
  def getArguments() : Option[InputResultPairViewInterface.Argument]

  def getRoot : Node = splitPane

}
