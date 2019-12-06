package planar_structure.subroutines

object FortranTest extends App{
    //lets test dopn
  val some = DOPN.DOPN(190, 5000, 5, 1, 2,ChangeableParameters.HRC(3),ChangeableParameters.HRC(2),
    ChangeableParameters.SGT1(2),ChangeableParameters.SGT2(2),2f,1,1)
  println(s"SGHD: ${some._1}\nSGMHD: ${some._2}\nSGF1D: ${some._3}\nSGF2D: ${some._4}\nSGFM1D: ${some._5}\nSGFM2D: ${some._6}")
  val some1 = ZUC2H.ZUC2H(24, 40, 0,0,ChangeableParameters.HRC(2),ChangeableParameters.HRC(2), 50,3,20,1.667f,1000,1,100,2,1,
    ChangeableParameters.HG, ChangeableParameters.BETFS,ChangeableParameters.ALF,ChangeableParameters.ALF,ChangeableParameters.ALF,
    ChangeableParameters.EPMI,ChangeableParameters.E1,ChangeableParameters.E2,ChangeableParameters.PUAS)
  println(s"1 : ${some1._1}")
  println(s"2 : ${some1._2}")
  println(s"3 : ${some1._3}")
  val some12 = new ZETHAB.B_Iterator(140)
  println(some12.toList)
/*  val ZA = new Iterator[Int]{
    var number: Int = 24
    override def hasNext: Boolean = if (number - za_corr > ZA_MIN) true else false
    override def next(): Int = {za_corr -= nw; za_corr}
  }
  println(ZA.toList)*/
  //val some2 = ZETHAB.W3(3, 24,3,0,0,0,0)
  Range(3, 10,1).foreach{(a : Int)=>
    println(ZETHAB.W3(a, 20,3,0,0,0,0))
  }
}
