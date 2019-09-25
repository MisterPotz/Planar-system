package planar_structure.mechanisms

import planar_structure.core_structure.{BaseLink, BaseMechanism, ChainLink, ChainLinkInterface, ExternalGearWheel}
import planar_structure.help_traits.{Addable, Updateable}

//basic type of mechanism, additional mechanism can be addded to existing one
trait Mechanism extends  BaseMechanism with Updateable with ChainLinkInterface[Mechanism] {
  //initial sequence of mechanism elements
  lazy val input_layer : ChainLink = new ChainLink()

  override def add[ChainLinkInterface](a: ChainLinkInterface): Mechanism.this.type = {
    //TODO input_layer.findOutput.add(a) //find the output and append a new mechanism to it
    this
  }

  override def addAll[ChainLinkInterface](all: ChainLinkInterface*): Mechanism.this.type = {
    //output - is a chainlink where the last output is located
    /*TODO def recursive_output(i : Int)(implicit prev_output : ChainLink) : Unit = {
      prev_output.findOutput.add(all(i))
      //finding new output
      recursive_output(i+1)(prev_output.findOutput)
    }
    recursive_output(0)(input_layer)*/
    this
  }

  override def getLink(i: Int): BaseLink = {
    new ExternalGearWheel() {}
  }
}