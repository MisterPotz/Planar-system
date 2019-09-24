package planar_structure.help_traits
import scala.collection.mutable.ArrayBuffer

trait FuncContainer[T]{
  implicit class AdditionalArray(arr : ArrayBuffer[T]){self =>
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