package planar_structure

import planar_structure.core_structure.{BaseGearWheel, ChainLink, ExternalConnection, ExternalGearWheel, InternalGearWheel, RecognizableBaseLink, Satellite}

object Main extends App with RecognizableBaseLink {
  println("Hi")
  val geared: ExternalConnection = new ExternalConnection(new ExternalGearWheel(), new ExternalGearWheel())
  val links = new ChainLink;
  links.addAll(new ExternalGearWheel(), new ExternalGearWheel(), new ExternalGearWheel(z = 200))
  //links.installConnections()
  val keksat: Satellite = new Satellite()

  keksat(0).addAll(new ExternalGearWheel(z = 1000), new InternalGearWheel())
  links.add(keksat)
  links.installConnections()
  println(links.connections_storage)
  println(keksat.crown_storage(0).connections_storage)
  keksat(0).getLink(0).asInstanceOf[BaseGearWheel].z = 2000
  println(keksat(0).getLink(0))
  println(keksat(0).getConnection(0))
  links.installConnections()
  println(links.connections_storage)
  println(keksat.crown_storage(0).connections_storage)
  println(keksat(0).getConnection(0))
  keksat.update()
  println(keksat)
}