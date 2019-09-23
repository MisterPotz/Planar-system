package planar_structure
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

//struct для хранения координат точек звеньев механизма на плоскости
case class Coordinates(var x : Double, var y: Double)

//sealed для того, чтобы сгенерировалась enum из классов
sealed  trait  BaseLink

trait BaseGearConnection extends  BaseLink

//базовая структура параметров колеса
abstract class BaseGearWheel(var z : Int,var  m: Double,var x:Double,var ha: Double = 1.0,var ca: Double =0.25,var alpha:Double=scala.math.toRadians(20.0)
                             ,var  axis_steady: Boolean = true,var rotates : Boolean = true) extends BaseLink {
  final def r : Double = m * z /2
  final def rb : Double = r * math.cos(alpha)

  override def toString: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates"

}

//базовая структура колеса и параметры с фиксированной осью и дозволенностью вращения
class ExternalGearWheel(z : Int = 30, m: Double = 1.25,x:Double = 0,
                              ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                             , axis_steady: Boolean = true, rotates : Boolean = true) extends BaseGearWheel(
                                                                                     z, m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = "\nExternal gear, parameters:"+ super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_ + _)

}

class InternalGearWheel( z : Int = 30,  m: Double = 1.25,  x:Double = 0,
                              ha: Double = 1.0, ca: Double =0.25, alpha:Double=scala.math.toRadians(20.0)
                             , axis_steady: Boolean = true, rotates : Boolean = true) extends BaseGearWheel(z,
                                                                                        m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = "\nInternal gear, parameters:"+ super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_ + _)
}
//TODO satellite thing
class Satellite extends BaseLink {
}
class Carrier extends  BaseLink
class Input extends BaseLink
class Output extends  BaseLink


sealed trait Addable[+A, U]{self =>
  def add[A <: U](a : A) : self.type
}
sealed trait Updateable[+A]{self=>
  def update() : self.type
}
sealed trait Conditionable[T]{self =>
  //using classtag so T is not erased during compilations
  def condition[U](elem : U)(implicit ct :  ClassTag[T]) : Boolean = {
    elem match {
      case elem: T => true
      case _ => false
    }
  }
}
trait GearConditionable extends  Conditionable[GearConnection[BaseGearWheel, BaseGearWheel]] {}
trait WheelConditionable extends  Conditionable[ BaseGearWheel] {}

trait ConnectionConstructor[T]{
  implicit class ArrayAddon(arr : ArrayBuffer[T]){self =>
    //принимает индекс подследнего "изъятого" элемента и пополняемый массив
    //разбивает на массив на группы по двое с общим элементом между группами
    def split_by_2_with_common(a: Int, arrayBuffer: ArrayBuffer[(T, T)] = new ArrayBuffer[(T, T)]()): ArrayBuffer[(T,T)] ={
      //если и так последний элемент - отставить и прекратить рекурсии
      if (a == arr.length - 1){
        arrayBuffer
      }
      else{
        arrayBuffer.addOne((arr(a), arr(a+1)))
        split_by_2_with_common(a+1, arrayBuffer)
      }
    }
  }
}

//basic chain of connections and wheels
class ChainLink extends Addable[BaseLink, BaseLink] with Updateable[BaseLink] with ConnectionConstructor[BaseLink]
with GearConnectionCreator {
//initializing array of mechanisms
  lazy protected val _mechanism_collection = new ArrayBuffer[BaseLink]()
  lazy val gear_condition: GearConditionable = new GearConditionable {}
  lazy val wheel_condition: WheelConditionable = new WheelConditionable {}
  //adding new element
  override def add[A <: BaseLink](a: A): ChainLink.this.type = {
    _mechanism_collection.addOne(a)
    this
  }
  //если надо поменять параметры колес - делаем новые, подразумевается что это делается после расчета нужных функций
  def update(collection : ChainLink) : ChainLink.this.type = {
    if (collection.getAllGears.length != getAllGears.length) throw new IllegalArgumentException("Collection size are not equal")
    else {
      for (this_gear <- 0 until _mechanism_collection.length){
        //переписываем колеса этой коллекции на колеса внешней коллекции
        _mechanism_collection(this_gear) = collection(this_gear)
      }
      this
    }
  }
  //очистить массив с колесами
  /*def clear() : ChainLink.this.type = {
    _mechanism_collection.mapInPlace()
  }*/
  //must be called only after all wheels are in
  def makeConnections() : ChainLink.this.type = {
    val args_for_connections  = _mechanism_collection.split_by_2_with_common(0)
    //creating indexed range for updated array
    val range = 1 until (_mechanism_collection.length + args_for_connections.length) by  2
    println(range)
    for ((wheels, i) <- args_for_connections.zip(range)){
      _mechanism_collection.insert(i, makeGearConnection(wheels._1.asInstanceOf[BaseGearWheel], wheels._2.asInstanceOf[BaseGearWheel]))
    }
    this
  }
  override def update(): ChainLink.this.type = {
    //после пополнения массива "чистыми колесами" сделать соединения между ними
    if (getAllConnections.isEmpty){
    }

    this
  }
  def getAllGears: ArrayBuffer[BaseLink] = {
    _mechanism_collection.filter(wheel_condition.condition)
  }
  def getAllConnections: ArrayBuffer[BaseLink] = {
    _mechanism_collection.filter(gear_condition.condition)
  }
  def hasElement(a : Int) : Boolean = if (a >= 0 & a < _mechanism_collection.length) true else false
  //returns wheel, not connnection
  def apply(a : Int): BaseLink = {
    if (hasElement(a))
      //return wheel a
      getAllGears(a)
    else
      throw new IllegalArgumentException("Out of bounds")
  }

  //TODO create mechanism from file (JSON)
  override def toString: String = "\nChain link, components: " concat _mechanism_collection.toString()
}

object SimpleTest extends ConnectionConstructor[Int]{
  lazy val arr: ArrayBuffer[Int] = ArrayBuffer(1,2,3,4,5,6,7,8,9,10)
  def test : ArrayBuffer[(Int, Int)] = {
    val testing_set = new ArrayBuffer[(Int, Int)]
    arr.split_by_2_with_common(0, testing_set)
    testing_set
  }
}

object Main extends App with GearConnectionCreator {
  println("-------------Hi, here is the test of ready objects/classes:-----------------\n")
  val links = new ChainLink; links.add(new ExternalGearWheel).add(new ExternalGearWheel()).add(new InternalGearWheel)
  println(links)
  links.makeConnections()
  println(links)
  /*println(geared match {case geared : GearConnection[BaseGearWheel, BaseGearWheel] => true; case _ => false })
  println(geared match {case geared : BaseGearWheel => true; case _ => false})*/
}
