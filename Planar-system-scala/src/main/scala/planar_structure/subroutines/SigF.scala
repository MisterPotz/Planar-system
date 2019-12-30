package planar_structure.subroutines

object SigF {

  case class ApproximateSigFTableRow(outerHBCheck: (StandardParameters.HBRange) => Boolean, innerHB: StandardParameters.HBRange => Boolean, sigFlim: (Double) => Double)

  def isInHBRangeFunc(min: Double, max: Double)(input: StandardParameters.HBRange): Boolean = {
    if (min <= (input.min + input.max) / 2 && max > (input.min + input.max) / 2) {
      true
    } else false
  }

  val ApproximateSigFTable = List(
    ApproximateSigFTableRow(isInHBRangeFunc(0, 350), isInHBRangeFunc(0, 350), 1.75 * _),
    ApproximateSigFTableRow(isInHBRangeFunc(480, 580), isInHBRangeFunc(250, 350), _ => 650),
    ApproximateSigFTableRow(isInHBRangeFunc(480, 550), isInHBRangeFunc(480, 550), _ => 550),
    //в ряду ниже не учитывается возможность автоматической регуляции при цементации - поэтому заведомо меньшие показатели могут выходить
    ApproximateSigFTableRow(isInHBRangeFunc(560, 630), isInHBRangeFunc(300, 450), _ => 800),
    //здесь в функции поиска sigflim нужна средняя hb сердцевины
    ApproximateSigFTableRow(isInHBRangeFunc(580, 670), isInHBRangeFunc(240, 400), 12 * _ / 10 + 290)
  )

  private def pickRowByMaterial(matrow: StandardParameters.MaterialTableRow): ApproximateSigFTableRow = {
    var ness_row = ApproximateSigFTable.find(row => row.outerHBCheck(matrow.outsideHB) && row.innerHB(matrow.insideHB))
    if (ness_row.isEmpty) {
      ness_row = ApproximateSigFTable.find(row => row.outerHBCheck(matrow.outsideHB))
    }
    ness_row.get
  }

  def getSigFLim(matrow: StandardParameters.MaterialTableRow): Double = {
    val ness_row = pickRowByMaterial(matrow)
    if (ness_row == ApproximateSigFTable(4)) {
      ness_row.sigFlim(matrow.outsideHB.mid)
    } else
      ness_row.sigFlim(matrow.outsideHB.mid)
  }

  def getSF(matrow: StandardParameters.MaterialTableRow): Double = {
    val ness_row = pickRowByMaterial(matrow)
    if (ness_row == ApproximateSigFTable(3)) {
      1.55
    } else {
      1.7
    }
  }

  import math.pow

  def getStressMode(approximateSigFTableRow: StandardParameters.MaterialTableRow): Int = {
    //примем пока его равным среднему равновероятностному
    2
  }

  def getYN(matrow: StandardParameters.MaterialTableRow, n: Double, nz: Int): Double = {
    val nk = SigH.getNK(n, nz)
    //после нахождения назначенного ресурса нужно учитываем типовой режим нагружения
    //примем пока его равным среднему равновероятностному
    val mode = getStressMode(matrow)
    val q = if (pickRowByMaterial(matrow) == ApproximateSigFTable(0)) 6 else 9
    val muf = StandardParameters.KFE(if (q == 6) 0 else 1)(mode)
    val nflim = 4e6
    val ynmax = 2.5
    val yn = pow(nflim / (nk * muf), 1 / q.toDouble)
    if (yn < 1) 1 else if (yn >= 1 && yn <= ynmax) yn else ynmax
  }

  def getYR(matrow: StandardParameters.MaterialTableRow): Double = {
    1
  }

  def getYA(matrow: StandardParameters.MaterialTableRow): Double = {
    val ness_row = pickRowByMaterial(matrow)
    if (ness_row == ApproximateSigFTable(0))
      0.65
    else if (ness_row == ApproximateSigFTable(4))
      0.9
    else
      0.75
  }

  def getDSigF(matrow: StandardParameters.MaterialTableRow, n: Double, nz: Int): Double = {
    getSigFLim(matrow) * getYN(matrow, n, nz) * getYR(matrow) * getYA(matrow) / getSF(matrow)
  }

  def fullFindDSigF(mat1: StandardParameters.MaterialTableRow, mat2: StandardParameters.MaterialTableRow, n1: Double,
                    n2: Double, v: Double, nz1: Int = 1, nz2: Int = 1) = {
    val first = getDSigF(mat1, n1, nz1)
    val second = getDSigF(mat2, n2, nz2)
    math.min(first, second)
  }

  def getKm(beta: Double): Double = {
    if (beta == 0) 3.4e3 else 2.8e3
  }

  def getKFv(materialTableRow: StandardParameters.MaterialTableRow, gradeAccuracy: Double, beta: Double): Double = {
    //TODO заглушечка
    1.3
  }

  def getKFBet(psiBA: Double, schemeType: Int, matrow: StandardParameters.MaterialTableRow): Double = {
    val khbet0 = CylindricGearTransmissionsCalculation.getKHBet0(psiBA, schemeType, matrow)
    0.18 + 0.82 * khbet0
  }

  def getKFAlph(v: Double, matrow1: StandardParameters.MaterialTableRow,
                matrow2: StandardParameters.MaterialTableRow,
                accuracyGrade: Int,
                bevel: Boolean = false): Double = {
    CylindricGearTransmissionsCalculation.getKHAlph(v, matrow1, matrow2, accuracyGrade, bevel)

  }

  def getKF(materialTableRow: StandardParameters.MaterialTableRow,
            matrow2: StandardParameters.MaterialTableRow,
            beta: Double,
            psiBA: Double, schemeType: Int,
            v: Double): Double = {
    val bevel = beta > 0
    val accuracyGrade = CylindricGearTransmissionsCalculation.getAccuracyGrade(v, bevel).toInt
    val kfv = getKFv(materialTableRow,accuracyGrade,beta)
    val kfbet = getKFBet(psiBA, schemeType, materialTableRow)
    val kfalph = getKFAlph(v, materialTableRow, matrow2, accuracyGrade, bevel)
    kfv * kfbet * kfalph
  }
}