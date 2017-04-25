package escher

import escher.SynNoOracle._
import DSL._
import Synthesis._


object TestSynNoOracle {
  def main(args: Array[String]): Unit = {


    val syn = new SynNoOracle(
      Config(maxCost = 20, logLevels = true, logReboot = true, logComponents = false, searchSizeFactor = 3),
      Console.print
    )

    import syn._

    def containsSynthesis() = {
      val args: IS[ArgList] = IS(
        argList(listValue(1,2,3), 1),
        argList(listValue(1,2,3), 2),
        argList(listValue(1,2,3), 3),
        argList(listValue(1,2,3), 4),
        argList(listValue(2,3), 3),
        argList(listValue(2,3), 2),
        argList(listValue(2,3), 1),
        argList(listValue(2,3), -1),
        argList(listValue(1), 1),
        argList(listValue(), 1)
      )

      val refComp = CommonComps.contains

      val examples = args.map(argList => argList -> refComp.executeEfficient(argList))

      synthesize(refComp.name, refComp.inputTypes, IS("xs", "x"), refComp.returnType)(envComps = CommonComps.standardComps, examples, CommonComps.rules_noTree)
    }

    def reverseSynthesis() = {
      val args: IS[ArgList] = IS(
        argList(listValue(1, 2, 3)),
        argList(listValue(1, 2)),
        argList(listValue(2, 3)),
        argList(listValue(1)),
        argList(listValue())
      )

      val refComp = CommonComps.reverse
      val examples = args.map(argList => argList -> refComp.executeEfficient(argList))

      synthesize(refComp.name, IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(envComps = CommonComps.standardComps, examples, CommonComps.rules_noTree)

    }

    TimeTools.printTimeUsed("Synthesis task") {
      reverseSynthesis() match {
        case Some(component) =>
          component.print()
          println(s"cost = ${component.cost}")
        case None => println("Failed")
      }
    }
  }
}
