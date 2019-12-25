package planar_structure.mechanism.process.report

case class FullConditionCheck(gearRatio : Float, alignment : Boolean,
                              assembly : Boolean, interferenceAll : Boolean,
                              neighborhood : Boolean, noPruning : Boolean)

//для кинематического анализа механизма
case class KinematicAnalysisReport(resU : Double,
                                  aw2 : Double,
                                  alignmentAccuracy: Double,
                                  maximumSize : Double,
                                   neighborhood : Boolean,
                                   assembly : Boolean,
                                   pruning : List[Boolean]//, ...
                                  )