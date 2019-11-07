package planar_interface.model

import planar_structure.mechanism.mech2kh.{ExternalInternal, MechanismType, MechanismTypeString}
import planar_structure.mechanism.{CarrierOutput, CarrierPosition, Mechanism, Mechanism2KH, MechanismFactory}

import scala.collection.mutable.HashMap
import scala.collection.{IterableOnce, mutable}
import scala.collection.parallel.immutable.ParVector

//subclasses must contain root view of the app and reference to the mechanism window


sealed trait CurrentMode;
case object KINEMATIC_ANALYSIS_FORWARD extends CurrentMode{
  override def toString: String = "KINEMATIC_ANALYSIS_FORWARD"
}
case object KINEMATIC_SYNTHESIS extends CurrentMode{
  override def toString: String = "KINEMATIC_SYNTHESIS"
}
case object STRENGTH_ANALYSIS_FORWARD extends CurrentMode{
  override def toString: String = "STRENGTH_ANALYSIS_FORWARD"
}
case object STRENGTH_SYNTHESIS extends CurrentMode{
  override def toString: String = "STRENGTH_SYNTHESIS"
}


trait ProcessUnit{

   def process_KINEMATIC_ANALYSIS_FORWARD(mechanism : Mechanism);
   def process_STRENGTH_ANALYSIS_FORWARD(mechanism : Mechanism);
   def process_STRENGTH_SYNTHESIS(mechanism : Mechanism);
   def process_KINEMATIC_SYNTHESIS(mechanism: Mechanism);

}

class ProcessUnitConcrete extends  ProcessUnit {
  //returns current status of execution

  def getCurrentStatus : Int = 100
  override def process_KINEMATIC_ANALYSIS_FORWARD(mechanism: Mechanism): Unit = {
    println("KINEMATIC_ANALYSIS_ACTIVATED")
    //TODO here must be a reference to a method in model structure

  }

  override def process_STRENGTH_ANALYSIS_FORWARD(mechanism: Mechanism): Unit = println("STERNGTH ANALYSIS ACTIVATED")

  override  def process_STRENGTH_SYNTHESIS(mechanism: Mechanism): Unit = println("STRENGTH SYNTHESIS ACTIVATED")

  override  def process_KINEMATIC_SYNTHESIS(mechanism: Mechanism): Unit = {
    //TODO must be also an input in the interface
    println("KINEMATIC SYNTHESIS ACTIVATED")
  }
}
sealed trait ModeArguments

/**
 *
 * @param z_min_max пара минимального и максимального значения числа зубьев для соответствующего колеса
 * @param m_arr массив допустимых модулей
 * @param k число сателлитов
 * @param n_input число оборотов входного колеса
 * @param u1h целевое передаточное отношение
 * @param eps_u1h требуемая точность передаточного отношения
 */
case class KinematicSynthesisArgs(z_min_max : Array[(Double, Double)], m_arr : Array[Double], k : Int, n_input : Double, u1h : Double, eps_u1h : Int)
//stores current mode and mechanism
class MechanismDatabase(val defaultFactory : MechanismFactory = Mechanism2KH) extends MechanismTypeString{

  protected val processUnit = new ProcessUnitConcrete
  var currentMode : CurrentMode = KINEMATIC_ANALYSIS_FORWARD;
  var currentType : (MechanismType, CarrierPosition) = (ExternalInternal, CarrierOutput)
  val mechanismDatabase : mutable.HashMap[(MechanismType, CarrierPosition), Mechanism] =
    mutable.HashMap.empty[(MechanismType, CarrierPosition), Mechanism]
  //current mechanism
  def mechanism : Mechanism = {
    mechanismDatabase(currentType)
  }
  //mechanism factory
  var mechanismFactory : MechanismFactory = Mechanism2KH
  init()
  def init() = {
    makeMechanism()
  }

  def setFactory(mechanismFactory: MechanismFactory) : Unit ={
    this.mechanismFactory = mechanismFactory
  }
  /**
   * if operation is successful, set mechanism to a newly baked one and return true
   * otherwise do nothing and return false
   * @param code string code of a mechanism
   * @return return boolean which indicates the success of an operation
   */
  def makeMechanism(code : String): Boolean ={
    mechanismDatabase.get(code.toFullType) match {
        //if we had such mechanism in the base before
      case Some(mechanism) =>
        println("Specialists found the same old mechanism")
        currentType = code.toFullType
        true
      case None => mechanismFactory.safeApply(code) match {
        case Left(_) => false
        case Right(value) =>
          println("new mechanism baked at Motherbase")
          //we must create the new mechanism lol, we must add it
          mechanismDatabase.addOne(code.toFullType -> value)
          currentType = code.toFullType
          true
      }
    }

  }
  //default mechanism - externalinternal with carier as an output
  def makeMechanism() : Boolean = {
    makeMechanism("ExternalInternal_CarrierOutput")
  }
  def processCurrentMode() : Unit = {
    //TODO make this process in another thread(s)
    currentMode match {
      case KINEMATIC_ANALYSIS_FORWARD => processUnit.process_KINEMATIC_ANALYSIS_FORWARD(mechanism)
      case KINEMATIC_SYNTHESIS => processUnit.process_KINEMATIC_SYNTHESIS(mechanism)
      case STRENGTH_ANALYSIS_FORWARD => processUnit.process_STRENGTH_ANALYSIS_FORWARD(mechanism)
      case STRENGTH_SYNTHESIS => processUnit.process_STRENGTH_SYNTHESIS(mechanism)
    }
  }
}
