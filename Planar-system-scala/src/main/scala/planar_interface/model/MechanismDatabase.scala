package planar_interface.model

import planar_structure.mechanism.{Mechanism, Mechanism2KH, MechanismFactory}

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
  override def process_KINEMATIC_ANALYSIS_FORWARD(mechanism: Mechanism): Unit = println("KINEMATIC_ANALYSIS_ACTIVATED")

  override def process_STRENGTH_ANALYSIS_FORWARD(mechanism: Mechanism): Unit = println("STERNGTH ANALYSIS ACTIVATED")

  override  def process_STRENGTH_SYNTHESIS(mechanism: Mechanism): Unit = println("STRENGTH SYNTHESIS ACTIVATED")

  override  def process_KINEMATIC_SYNTHESIS(mechanism: Mechanism): Unit = println("KINEMATIC SYNTHESIS ACTIVATED")
}

//stores current mode and mechanism
class MechanismDatabase(val defaultFactory : MechanismFactory = Mechanism2KH) {

  protected val processUnit = new ProcessUnitConcrete
  var currentMode : CurrentMode = KINEMATIC_ANALYSIS_FORWARD;
  //current mechanism
  var mechanism : Mechanism = _
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
    mechanismFactory.safeApply(code) match {
      case Left(value) => false
      case Right(value) => { mechanism = value; true}
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
