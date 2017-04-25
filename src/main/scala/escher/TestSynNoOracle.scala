package escher

import escher.SynNoOracle._
import DSL._
import Synthesis._
import escher.CommonComps.ReducibleCheck


object TestSynNoOracle {
  def main(args: Array[String]): Unit = {


    val syn = new SynNoOracle(
      Config(maxCost = 20, logLevels = true, logReboot = true, logComponents = true, searchSizeFactor = 3),
      Console.print
    )

    import syn._

    def synthesizeUsingRef(refComp: ComponentImpl, argNames: IS[String],
                           exampleInputs: IS[ArgList],
                           additionalComps: (Set[ComponentImpl], Map[ComponentImpl, ReducibleCheck]) = (Set(), Map())) = {
      val examples = exampleInputs.map(argList => argList -> refComp.executeEfficient(argList))
      val additionalImpls = additionalComps._1
      val additionalRules = additionalComps._2

      synthesize(refComp.name, refComp.inputTypes, argNames, refComp.returnType)(
        envComps = CommonComps.standardComps ++ additionalImpls, examples, CommonComps.rules_noTree ++ additionalRules)
    }

    def reverseSynthesis() = {
      synthesizeUsingRef(CommonComps.contains, IS("xs"), exampleInputs = IS(
        argList(listValue(1, 2, 3)),
        argList(listValue(1, 2)),
        argList(listValue(2, 3)),
        argList(listValue(1)),
        argList(listValue())
      ))
    }

    def stutterSynthesis() = {
      synthesizeUsingRef(CommonComps.stutter, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(5)),
        argList(listValue(5, 6, 3))
      ))
    }

    def containsSynthesis() = {
      synthesizeUsingRef(CommonComps.contains, IS("xs", "x"), exampleInputs = IS(
//        argList(listValue(1,2,3), 4),
//        argList(listValue(1,2,3), 3),
//        argList(listValue(1,2,3), 2),
//        argList(listValue(1,2,3), 1),
//        argList(listValue(2,3), 3),
//        argList(listValue(2,3), 2),
//        argList(listValue(2,3), 1),
//        argList(listValue(2,3), -1),
//        argList(listValue(1), 1),
//        argList(listValue(), 1)
        argList(listValue(1,2,3), 1),
        argList(listValue(1,2,3), 2),
        argList(listValue(1,2,3), 3),
        argList(listValue(1,2,3), 4),
        argList(listValue(1,2,3), -1),
        argList(listValue(1,2), 3),
        argList(listValue(), 1)
      ))
    }

    def cartesianSynthesis() = {
      synthesizeUsingRef(CommonComps.cartesian, IS("xs", "ys"), exampleInputs = IS(
        argList(listValue(), listValue(2, 3, 4)),
        argList(listValue(5), listValue()),
        argList(listValue(5), listValue(7, 8, 9)),
        argList(listValue(2, 3), listValue(4, 5))
      ), additionalComps = (Set(CommonComps.createPair(tyFixVar(0), tyFixVar(1))), Map()))
    }

    def squareListSynthesis() = {
      synthesizeUsingRef(CommonComps.squareList, IS("n"), exampleInputs = IS(
        argList(-3),
        argList(0),
        argList(1),
        argList(2),
        argList(3),
        argList(4)
      ), additionalComps = (CommonComps.timesAndDiv, CommonComps.rules_timesAndDiv))
    }

    TimeTools.printTimeUsed("Synthesis task") {
      containsSynthesis() match {
        case Some(component) =>
          component.print()
          println(s"cost = ${component.cost}")
        case None => println("Failed")
      }
    }
  }
}
