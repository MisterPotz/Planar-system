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
abstract class BaseGearWheel(val z : Int,val  m: Double,val x:Double,val ha: Double = 1.0,val ca: Double =0.25,val alpha:Double=scala.math.toRadians(20.0)
                             ,val  axis_steady: Boolean = true,val rotates : Boolean = true) extends BaseLink {
  final val r : Double = m * z /2
  final val rb : Double = r * math.cos(alpha)

  override def toString: String = s"z: $z\nm: $m\nx: $x\nha: $ha\nca: $ca\nalpha: $alpha\naxis_steady: $axis_steady\nrotates: $rotates"

}

//базовая структура колеса и параметры с фиксированной осью и дозволенностью вращения
case class ExternalGearWheel(override val z : Int = 30, override val m: Double = 1.25, override val x:Double = 0,
                             override val ha: Double = 1.0,override val ca: Double =0.25,override val alpha:Double=scala.math.toRadians(20.0)
                             ,override val axis_steady: Boolean = true,override val rotates : Boolean = true) extends BaseGearWheel(
                                                                                     z, m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = "\nExternal gear, parameters:"+ super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_ + _)

}

case class InternalGearWheel(override val z : Int = 30, override val m: Double = 1.25, override val x:Double = 0,
                             override val ha: Double = 1.0,override val ca: Double =0.25,override val alpha:Double=scala.math.toRadians(20.0)
                             ,override val axis_steady: Boolean = true,override val rotates : Boolean = true) extends BaseGearWheel(z,
                                                                                        m,x,ha,ca,alpha, axis_steady, rotates){
  override def toString: String = "\nInternal gear, parameters:"+ super.toString.split("\n").map(_ + "\n\t").foldLeft("\n\t")(_ + _)
}
class Satellite extends BaseLink
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
    def split_by_2_with_common(a: Int, arrayBuffer: ArrayBuffer[( T, T)]): ArrayBuffer[(T,T )] ={
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
class ChainLink extends Addable[BaseLink, BaseLink] with Updateable[BaseLink] {
//initializing array of mechanisms
  lazy protected val _mechanism_collection = new ArrayBuffer[BaseLink]()
  lazy val gear_condition: GearConditionable = new GearConditionable {}
  lazy val wheel_condition: WheelConditionable = new WheelConditionable {}
  //adding new element
  override def add[A <: BaseLink](a: A): ChainLink.this.type = {
    _mechanism_collection.addOne(a)
    this
  }
  override def update(): ChainLink.this.type = {
    this
  }
  def getAllGears: ArrayBuffer[BaseLink] = {
    _mechanism_collection.filter(wheel_condition.condition)
  }
  def getAllConnections: ArrayBuffer[BaseLink] = {
    _mechanism_collection.filter(gear_condition.condition)
  }
  def hasElement(a : Int) : Boolean = if (a >= 0 & a < _mechanism_collection.length) true else false
  def apply(a : Int): BaseLink = {
    if (hasElement(a))
      _mechanism_collection(a)
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
  println("Hi")
  println(SimpleTest.test)
  println(ExternalGearWheel().toString)
  implicit def kek(externalConnection: ExternalConnection) : ExternalConnection = externalConnection

  val geared : ExternalConnection = new ExternalConnection(ExternalGearWheel(), ExternalGearWheel())
  val links = new ChainLink; links.add(ExternalGearWheel()).add(geared).add(ExternalGearWheel())
  println(links)
  println(links.getAllConnections)
  println(links.getAllGears)
  /*println(geared match {case geared : GearConnection[BaseGearWheel, BaseGearWheel] => true; case _ => false })
  println(geared match {case geared : BaseGearWheel => true; case _ => false})*/
}
