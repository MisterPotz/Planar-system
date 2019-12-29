package planar_interface.model

import javafx.application.Platform
import planar_interface.Observable
import planar_interface.view.event_types.{CalculatedKinematicForward, CalculatedKinematicSynthesis, CalculationFailed, EventListener}
import planar_structure.mechanism.mech2kh.Mechanism2KH
import planar_structure.mechanism.process.actors.{KinematicSynthesisProcessor, KinematicSynthesisProcessorInterface}
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.process.report.KinematicSynthesisReport
import planar_structure.mechanism.types._
import planar_structure.mechanism.{Mechanism, MechanismFactory, types}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}


//stores current mode and mechanism
class MechanismDatabase(val defaultFactory : MechanismFactory = Mechanism2KH) extends CodeGenerator with Observable{
  //protected val processUnit = null //new ProcessUnitConcrete
  var currentMode : String = ProcessType.KINEMATIC_ANALYSIS_FORWARD
  var currentType : String =  CodeGenerator(ExternalInternal, CarrierOutput)
  val mechanismDatabase : mutable.HashMap[String, Mechanism] =
    mutable.HashMap.empty[String, Mechanism]
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
    mechanismDatabase.get(code) match {
        //if we had such mechanism in the base before
      case Some(mechanism) =>
        println("Specialists found the same old mechanism")
        currentType = code
        true
      case None => mechanismFactory.safeApply(code) match {
        case Left(_) =>
          false
        case Right(value) =>
          println("new mechanism baked at Motherbase")
          //we must create the new mechanism lol, we must add it
          mechanismDatabase.addOne(code -> value)
          currentType = code
          true
      }
    }

  }
  //default mechanism - externalinternal with carier as an output
  def makeMechanism() : Boolean = {
    makeMechanism("ExternalExternal_CarrierOutput")
  }
  def stopProcessCurrentMode() : Unit = {
    processorBoi.stopWhatever()
    Platform.runLater(() => {
      notifyObservers(CalculationFailed)
    })
  }
  val processorBoi : ProcessUnitInterface = new ProcessUnitInterface {}
  def processCurrentMode(args: AnyRef = null) : Unit = {
    currentMode match {
      case ProcessType.KINEMATIC_ANALYSIS_FORWARD =>
        //getting result, this is a fast operation so we don't do threading
        //val res = mechanism.methods.fullConditionCheck
        //notifying everyone that we have processed the request
        //notifyObservers(CalculatedKinematicForward(res))
      case ProcessType.KINEMATIC_SYNTHESIS =>
        val callback : (KinematicSynthesisReport) => Unit = (report) => {
          Platform.runLater(() => {
            notifyObservers(CalculatedKinematicSynthesis(report))
          })
        }
        processorBoi.performKinematicSynthesis(args.asInstanceOf[KinematicSynthesisArgs], callback)
      case _ => println("unknown type of process")
    }
    /*//TODO make this process in another thread(s)
    currentMode match {
      case KINEMATIC_ANALYSIS_FORWARD => processUnit.process_KINEMATIC_ANALYSIS_FORWARD(mechanism)
      case KINEMATIC_SYNTHESIS => processUnit.process_KINEMATIC_SYNTHESIS(mechanism)
      case STRENGTH_ANALYSIS_FORWARD => processUnit.process_STRENGTH_ANALYSIS_FORWARD(mechanism)
      case STRENGTH_SYNTHESIS => processUnit.process_STRENGTH_SYNTHESIS(mechanism)
    }*/
  }
}

object MechanismDatabase{
  var mechanismDB = new MechanismDatabase()
  def getAvailableDB : Option[MechanismDatabase] = {
    Option(mechanismDB)
  }
}