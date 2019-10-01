package planar_structure

import planar_structure.core_structure.{BaseGearWheel, BaseLink, ChainLink, ExternalConnection, ExternalGearWheel, InternalGearWheel, RecognizableBaseLink, Satellite}
import planar_structure.help_traits.BeautifulDebugOutput
import planar_structure.mechanisms.{Mechanism, MechanismTypes}
class Tests extends BeautifulDebugOutput{
  override def toString: String = print("whoa\ncool")
}
class subTest extends Tests{
  override def toString: String = print(super.toString)
}

object Main extends App with RecognizableBaseLink {
  println("Planar-system initial structure test")
  val links = new ChainLink;
  links.addAll(new ExternalGearWheel(), new ExternalGearWheel(z = 40), new ExternalGearWheel(z = 200))
  //links.installConnections()
  val keksat: Satellite = new Satellite()
  val keksatted: Satellite = new Satellite()
  keksatted.addChainLink(0, new ExternalGearWheel(z = 400), new InternalGearWheel(z = 1000))
  keksat.addChainLinks(
    (-1, new ChainLink(new ExternalGearWheel(z = 1000), new InternalGearWheel(z = 350), new ExternalGearWheel(z = 100))),
    (0, new ChainLink(keksatted)),
    (1, new ChainLink(new ExternalGearWheel(z = 30))))
  links.add(keksat)
  links.installConnections()
  println(s"Total chain branch size: ${links.getBranchSize}")
  println(s"Satellite test size: ${keksat.getBranchSize}")
  val mechanism: Mechanism = new Mechanism(links)
  //val chainLink_new : ChainLink = links.copy //let's just hope this copying mechanism works
  mechanism.installConnections()
  println(mechanism)
  val test =  mechanism.input_layer.getAllLinksAllowedForConnection
  test.foreach((f: BaseLink) => println(f))
  println(test.length)
}