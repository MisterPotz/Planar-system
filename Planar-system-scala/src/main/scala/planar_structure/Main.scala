package planar_structure

import planar_structure.core_structure.{BaseGearWheel, BaseLink, ChainLink, ExternalConnection, ExternalGearWheel, GearConnection, InternalGearWheel, RecognizableBaseLink, Satellite}
import planar_structure.help_traits.{BeautifulDebugOutput, StorageHashDConnection}
import planar_structure.mechanisms.{Mechanism, MechanismTypes, OneRowClassic}
class Tests extends BeautifulDebugOutput{
  override def toString: String = print("whoa\ncool")
}
class subTest extends Tests{
  override def toString: String = print(super.toString)
}

object Main extends App with RecognizableBaseLink {
  /*println("Planar-system initial structure test")
  val links = new ChainLink(new StorageHashDConnection {});
  links.addAll(new ExternalGearWheel(), new ExternalGearWheel(z = 40), new ExternalGearWheel(z = 200))
  //links.installConnections()
  val keksat: Satellite = new Satellite(links.getConnectionStorage)
  keksat.addChainLinks(
    (-1, new ChainLink(keksat.getConnectionStorage ,new ExternalGearWheel(z = 1000), new ExternalGearWheel(z = 100))),
    (0, new ChainLink(keksat.getConnectionStorage, new InternalGearWheel(z = 400))),
    (1, new ChainLink(keksat.getConnectionStorage, new ExternalGearWheel(z = 30), new InternalGearWheel(z = 500))))
  links.add(keksat)
  links.installConnections()
  println(s"Total chain branch size: ${links.getBranchSize}")
  println(s"Satellite test size: ${keksat.getBranchSize}")
  println(links.toStringShort)
  //println(s"All connections: \n${GearConnection.connection_storage.toStringShortSmart(() => links.connectionsSorter)}")
  println(s"Installed connections: ${links.getAllLinksAllowedForConnectionFull.length}")
  println("Connections types, sorted:")
  links.getAllLinksAllowedForConnectionFull.foreach((a : (BaseLink,BaseLink)) => {println(a._1.toStringShort + "\t" + a._2.toStringShort)})
  for (i <- 0 until links.getConnectionsAmount){
    println(links.getConnection(i).toString + "\n")
  }*/

  //TODO функция нахождения пригодных для соединений звеньев работает, но лучше потестить
  //println(links)
  /*val mechanism: Mechanism = new Mechanism(links)
  //val chainLink_new : ChainLink = links.copy //let's just hope this copying mechanism works
  mechanism.installConnections()
  println(mechanism)
  val test =  mechanism.input_layer.getAllLinksAllowedForConnectionFull
  val some_connection = mechanism.getConnection(0)*/
  val mechanism = new OneRowClassic()
  println(mechanism.toStringShort)
}