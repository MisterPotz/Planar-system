package planar_interface.model

import java.util.Calendar

import planar_interface.view.event_types.EventListener
import planar_structure.mechanism.Mechanism
import planar_structure.mechanism.process.actors.{KinematicSynthesisProcessor, KinematicSynthesisProcessorInterface}
import planar_structure.mechanism.process.argument.KinematicSynthesisArgs
import planar_structure.mechanism.process.report.KinematicSynthesisReport


trait ProcessUnitInterface{
  def stopWhatever(): Unit = {
    processor.stop()
  }

  var processor : KinematicSynthesisProcessorInterface = _
  def performKinematicSynthesis(args : KinematicSynthesisArgs,
                                callbackfunction: (KinematicSynthesisReport) => Unit) : Unit = {
    //получаем аргумент для расчета
    //настраиваем интерйфес для обратной связи, что будем делать при получении рещзультата
    val callback : EventListener = new EventListener {
      override def callback(usefulInfo: AnyRef): Unit = {
        //получаем репорт после обсчета
        val report = usefulInfo.asInstanceOf[KinematicSynthesisReport]
        //TODO sort mechanisms by size
        val sorted : Array[Mechanism] = report.sorted_mechanism.sortWith(
          (left, right) => left.gearStructureCharacteristic.getMaxStageSize <
            right.gearStructureCharacteristic.getMaxStageSize)
        sorted.foreach(mech => {
          print("Mechanism\t")
          mech.getGears.foreach(gear => {
            print(s"gear z:${gear.holder.z}\t")
          })
          print(s"Stage size d: ${mech.gearStructureCharacteristic.getMaxStageSize}")
          //println(s"gear ratio: ${mech.methods.getGearRatio}")
          println(s"gear ratio: ${mech.calculator.findU(mech)}")
        })
        val now = Calendar.getInstance()
        println(s"Totally mechanisms: ${sorted.length}")
        println(s"Execution ended: ${now.getTime}")
        callbackfunction(KinematicSynthesisReport(sorted))
      }
    }
    //кто будет считать, ну или командовать расчетом
    processor = new KinematicSynthesisProcessor(args)
    //передаем колбэек-интерфейс
    processor.setCompletionListener(callback)
     val now = Calendar.getInstance()
    println(s"Execution started: ${now.getTime}")
    processor.startComputation()
  }
}
