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
        argList(listValue(1,2,3), -1),
        argList(listValue(1,2), 3),
        argList(listValue(), 1)
      )

      val refComp = CommonComps.contains
      val name = "contains"

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(name, refComp.inputTypes, IS("xs", "x"), refComp.returnType)(envCompMap = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    containsSynthesis()
  }
}
