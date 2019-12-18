package planar_structure.subroutines

import jdk.dynalink.StandardNamespace

object StandardParameters {
  /**
   * ПРЕДЕЛЫ ТЕКУЧЕСТИ МАТЕРИАЛОВ КОЛЕС
   */
  val SGT = Array(700, 780, 800, 590, 780, 780, 700, 700, 780)
  val SGTA = SGT.slice(0, 3)
  val SGTG = SGT.slice(3, 6)
  val SGTB = SGT.slice(6, 9)

  def findNearest(array: Array[Double], target: Double) : Double = {
    val ind = array.findLast(_ < target).get
    val index = array.indexOf(ind)
    if (index < array.length - 1){
      array(index + 1)
    } else {
      array(index)
    }
  }

  /**
   * РЯД МОДУЛЕЙ ПО ГОСТ
   */
  val MS = Array(0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5,
    6, 7, 8)
  /**
   * КОЭФФИЦИЕНТЫ ПРИВЕДЕНИЯ ПЕРЕМЕННОГО РЕЖИМА НАГРУЖЕНИЯ К ПОСТОЯННОМУ
   */
  val KFE = Array(Array(1f, 0.3f, 0.143f, 0.065f, 0.038f, 0.013f),
    Array(1f, 0.2f, 0.1f, 0.036f, 0.016f, 0.004f))
  val KHE = Array(1f, 0.5f, 0.25f, 0.18f, 0.125f, 0.063f)
  /**
   * Ряд стандартных чисел RA40
   */
  val RA40S = Array(10f, 10.5f, 11f, 11.5f, 12f, 13f, 14f, 15f, 16f, 17f, 18f, 19f,
    20f, 21f, 22f, 24f, 25f, 26f, 28f, 30f, 32f, 34f, 36f, 38f, 40f, 42f, 45f, 48f, 50f, 53f, 56f, 60f,
    63f, 67f, 71f, 75f, 80f, 85f, 90f, 95f, 100f, 105f, 110f, 120f, 125f, 130f, 140f, 150f, 160f, 170f,
    180f, 190f, 200f, 210f, 220f, 240f, 250f, 260f, 280f, 300f, 320f, 340f, 360f, 380f, 400f, 420f, 450f,
    480f, 500f, 530f, 560f, 600f, 630f, 670f, 710f, 750f, 800f, 850f, 900f, 950f)
  /**
   * Коэффициент учитывает приложение нагрузки в зависимости от материала
   * при односторонней нагрузке - 1
   * при улучшенной стали - 0.65
   * при закалке ТВЧ - 0.75
   * при цементированной стали - 0.9
   * в подпрограмме DOPN, если KFC не равен единице, значение коэфф-та автоматически
   * подберется по твердости колеса
   */
  val KFC = Array(0.65f, 0.75f, 0.9f, 1f)
  /**
   * массив индексов типового нрежима нагружения передачи
   * 0 - постоянный
   * 1 - тяжелый
   * 2 - средний равновероятный
   * 3 - средний нормальный
   * 4 - легкий
   * 5- особо легкий
   * режим передачи отражает зависимость отношения (T_i / T_max) от (N_i / N_sum)
   * необходимо выбрать какой режим использовать для каждого зацепления (одинаковый)
   * можно переложить выбор пользователю
   */
  val NRR = (0, 1, 2, 3, 4, 5)

  val StMarks: Map[Int, String] = Map(0 -> "45", 1 -> "40Х", 2 -> "40ХН", 3 -> "35ХМ", 4 -> "40ХН2МА", 5 -> "38Х2МЮА", 6 -> "20Х",
    7 -> "20ХН2М", 8 -> "18ХГТ", 9 -> "12ХН3А", 10 -> "25ХГР")
  val TermicProcessTypes: Map[Int, String] = Map(0 -> "Улучшение", 1 -> "Улучшение и закалка ТВЧ", 2 -> "Улучшение и азотирование",
    3 -> "Улучшение, цементация и закалка")

  case class HBRange(min: Double, max: Double) {
    def mid : Double = {
      (min + max) / 2
    }
  }

  case class MaterialTableRow(StMark: List[String], processType: String, DMax: Double, SMax: Double, insideHB: HBRange,
                              outsideHB: HBRange, SigmaT: Double)

  val HBRangeVariants = Map(0 -> HBRange(235, 262), 1 -> HBRange(269, 302), 2 -> HBRange(450, 580), 3 -> HBRange(480, 550),
    4 -> HBRange(580, 670), 5 -> HBRange(560, 630), 6 -> HBRange(300, 400))
  val MaterialsTable: List[MaterialTableRow] = List(
    MaterialTableRow(List(StMarks(0)), TermicProcessTypes(0), 125, 80, HBRangeVariants(0), HBRangeVariants(0), 540),
    MaterialTableRow(List(StMarks(0)), TermicProcessTypes(0), 80, 50, HBRangeVariants(1), HBRangeVariants(1), 650),
    //-------
    MaterialTableRow(List(StMarks(1)), TermicProcessTypes(0), 200, 125, HBRangeVariants(0), HBRangeVariants(1), 640),
    MaterialTableRow(List(StMarks(1)), TermicProcessTypes(0), 125, 80, HBRangeVariants(1), HBRangeVariants(1), 750),
    MaterialTableRow(List(StMarks(1)), TermicProcessTypes(1), 125, 80, HBRangeVariants(1), HBRangeVariants(2), 750),
    //-------
    MaterialTableRow(List(StMarks(2), StMarks(3)), TermicProcessTypes(0), 315, 200, HBRangeVariants(0), HBRangeVariants(0), 630),
    MaterialTableRow(List(StMarks(2), StMarks(3)), TermicProcessTypes(0), 200, 125, HBRangeVariants(1), HBRangeVariants(1), 750),
    MaterialTableRow(List(StMarks(2), StMarks(3)), TermicProcessTypes(1), 200, 125, HBRangeVariants(1), HBRangeVariants(3), 750),
    //-------
    MaterialTableRow(List(StMarks(4), StMarks(5)), TermicProcessTypes(2), 125, 80, HBRangeVariants(0), HBRangeVariants(4), 780),
    MaterialTableRow(List(StMarks(6), StMarks(7), StMarks(8), StMarks(9), StMarks(10)),
      TermicProcessTypes(3), 200, 125, HBRangeVariants(6), HBRangeVariants(5), 800)
  )

  //материалы 1 - колеса, 2 - шестерни, выдает списком все доступные варианты
  def getMaterialsByProcessMode(i: Int): List[(MaterialTableRow, MaterialTableRow)] = {
    i match {
      case 1 =>
        MaterialsTable.filter(row => TermicProcessTypes(0) == row.processType && row.outsideHB == HBRangeVariants(0)).flatMap(row =>
          MaterialsTable.filter(row_second => TermicProcessTypes(0) == row_second.processType && row_second.outsideHB == HBRangeVariants(1)).map(row2 =>
            (row, row2)))
      case 2 =>
        MaterialsTable.filter(row => TermicProcessTypes(0) == row.processType && row.outsideHB == HBRangeVariants(1)).flatMap(row =>
          MaterialsTable.filter(row_second => TermicProcessTypes(1) == row_second.processType).map(row2 =>
            (row, row2))).filter(pair => pair._1 == pair._2)
      case 3 =>
        MaterialsTable.filter(row => TermicProcessTypes(1) == row.processType).flatMap(row =>
          MaterialsTable.filter(row_second => TermicProcessTypes(1) == row_second.processType).map(row2 =>
            (row, row2))).filter(pair => pair._1.StMark == pair._2.StMark)
      case 4 =>
        MaterialsTable.filter(row => TermicProcessTypes(1) == row.processType).flatMap(row =>
          MaterialsTable.filter(row_second => TermicProcessTypes(3) == row_second.processType).map(row2 =>
            (row, row2)))
      case 5 =>
        MaterialsTable.filter(row => TermicProcessTypes(3) == row.processType).flatMap(row =>
          MaterialsTable.filter(row_second => TermicProcessTypes(3) == row_second.processType).map(row2 =>
            (row, row2))).filter(pair => pair._1.StMark == pair._2.StMark)
    }
  }
}



object StandardParameterTest extends App {
  for (i <- Range(1, 6)) {
    println(StandardParameters.getMaterialsByProcessMode(i))
  }
}


