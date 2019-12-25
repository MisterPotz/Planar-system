package planar_structure.subroutines

object SigH {
  sealed trait SteelType

  case object CarbonizedLigated extends SteelType

  case object Ligated extends SteelType

  case class SigHLibTableRow(processType: String, averageHBSurface: (Double) => Boolean, steelType: SteelType, sigHLimFunc: (Double) => Double)

  val Table = List(
    SigHLibTableRow(StandardParameters.TermicProcessTypes(0), _ < 350, CarbonizedLigated, 2 * _ + 70),
    SigHLibTableRow(StandardParameters.TermicProcessTypes(1), a => 400 < a && a < 500, CarbonizedLigated, 17 * _ / 10 + 200),
    SigHLibTableRow(StandardParameters.TermicProcessTypes(3), _ > 560, Ligated, 23 / 10 * _),
    SigHLibTableRow(StandardParameters.TermicProcessTypes(2), _ > 52, Ligated, _ => 1050)
  )

  def getSigHLibTableRow_byMaterial(matrow: StandardParameters.MaterialTableRow): SigHLibTableRow = {
    Table.find(row => row.averageHBSurface((matrow.outsideHB.min + matrow.outsideHB.max) / 2)).get
  }

  def getSigHlim(matrow: StandardParameters.MaterialTableRow): Double = {
    getSigHLibTableRow_byMaterial(matrow).sigHLimFunc((matrow.outsideHB.max + matrow.outsideHB.min) / 2)
  }

  def getSH(matrow: StandardParameters.MaterialTableRow): Double = {
    if (matrow.processType == StandardParameters.TermicProcessTypes(0)) {
      1.1
    } else {
      //здесь принимается что любая закалка не ОБЪЕМНАЯ, хотя ТВЧ может в зависимости от размера быть и объемной
      1.2
    }
  }

  def getNK(n: Double, nz: Int, LH: Int = ChangeableParameters.LH /*число лет работы, принимаем за 160000 */): Double = {
    60 * n * nz * LH
  }

  import math.pow

  def getNHlim(materialTableRow: StandardParameters.MaterialTableRow): Double = {

    val nhlim = 30 * pow((materialTableRow.outsideHB.min + materialTableRow.outsideHB.max) / 2, 2.4)
    if (nhlim > 12e7)  12e7/*throw new IllegalArgumentException(s"${SigH.getClass.getName}: too high average HB")*/
    else
      nhlim
  }

  def getZNMax(materialTableRow: StandardParameters.MaterialTableRow): Double = {
    if (materialTableRow.processType == StandardParameters.TermicProcessTypes(0)) {
      2.6
    } else {
      //здесь принимается что любая закалка не ОБЪЕМНАЯ, хотя ТВЧ может в зависимости от размера быть и объемной
      1.8
    }
  }

  def getZN(matrow: StandardParameters.MaterialTableRow, n: Double, nz: Int): Double = {
    val nk = getNK(n, nz)
    val mode = getStressMode(matrow)
    val nhlim = getNHlim(matrow)
    val znmax = getZNMax(matrow)
    if (nk <= nhlim) {
      val zn = pow(nhlim / (nk * StandardParameters.KHE(mode)), 1 / 6)
      if (zn <= znmax) zn else znmax
    }
    else {
      val zn = pow(nhlim / (nk * StandardParameters.KHE(mode)), 1 / 20)
      if (zn >= 0.75) zn else znmax
    }
  }

  def getZR(matrow: StandardParameters.MaterialTableRow): Double = {
    0.95 //не заморачиваемся с коэффициентом от шероховатости
  }

  //стандартно принимается что механизм работает на медленном режиме (скорость меньше пяти метров в секунду)
  def getZV(matrow: StandardParameters.MaterialTableRow, v: Double = 5): Double = {
    if (v <= 5) {
      1.1
    } else {
      if ((matrow.outsideHB.min + matrow.outsideHB.max) / 2 <= 350) {
        val zv = 0.85 * pow(v, 0.1)
        if (zv >= 1) zv else 1
      } else {
        val zv = 0.925 * pow(v, 0.05)
        if (zv >= 1) zv else 1
      }
    }
  }

  def chooseDSigH(dsigh1: Double, dsigh2: Double, beta: Double): Double = {
    if (beta == 0)
      math.min(dsigh1, dsigh2)
    else {
      val dsigh = math.sqrt(pow(dsigh1, 2) + pow(dsigh2, 2))
      if (dsigh <= 1.2 * math.min(dsigh1, dsigh2)) {
        dsigh
      } else 1.2 * math.min(dsigh1, dsigh2)
    }
  }

  def getDSigH(materialTableRow: StandardParameters.MaterialTableRow, n: Double, nz: Int, v: Double): Double = {
    getSigHlim(materialTableRow) * getZN(materialTableRow, n, nz) * getZR(materialTableRow) * getZV(materialTableRow, v) / getSH(materialTableRow)
  }

  def getStressMode(approximateSigFTableRow: StandardParameters.MaterialTableRow): Int = {
    //примем пока его равным среднему равновероятностному
    2
  }

  def fullFindDSigH(mat1 : StandardParameters.MaterialTableRow, mat2 : StandardParameters.MaterialTableRow,
                    n1 : Double, n2 : Double, v: Double, beta : Double,  nz1 : Int = 1, nz2 : Int = 1) = {
    val first = getDSigH(mat1, n1, nz1, v)
    val second = getDSigH(mat2, n2, nz2, v)
    chooseDSigH(first,second, beta)
  }

  //def getASigH()
}