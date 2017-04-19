package escher

import escher.Synthesis.ArgList


/**
  * Created by weijiayi on 04/04/2017.
  */
object Main {

  def testDivide(): Unit ={
    import escher.Synthesis._

    val a = divideNumberAsSum(2000,3,0)
    println{
      a.length
    }
  }

  def testCartesianProduct(): Unit = {
    import escher.Synthesis._
    val r = cartesianProduct(IndexedSeq(Map(1->2,3->4), Map(5->6), Map(7->8, 9->10)))
    println(
      r.toList
    )
  }


  def testSynthesisTyped(): Unit = {
    import escher.SynthesisTyped._
    import DSL._

    val syn = new SynthesisTyped(Config(maxCost = 20, logLevels = false, logReboot = true, logComponents = false), Console.print)
    import syn._

    def reverseSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(2, 3, 4)) -> listValue(4, 3, 2)
      )

      val refComp = CommonComps.reverse

      synthesize("reverse", IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def stutterSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(5)) -> listValue(5, 5),
        argList(listValue(5, 6, 3)) -> listValue(5, 5, 6, 6, 3, 3)
      )

      val refComp = CommonComps.stutter

      synthesize("stutter", IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def cartesianSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue(), listValue(2, 3, 4)) -> listValue(),
        argList(listValue(5), listValue()) -> listValue(),
        argList(listValue(5), listValue(7, 8, 9)) -> listValue((5, 7), (5, 8), (5, 9)),
        argList(listValue(2, 3), listValue(4, 5)) -> listValue((2, 4), (2, 5), (3, 4), (3, 5))
      )

      val refComp = CommonComps.cartesian

      synthesize("cartesian", IS(tyList(tyVar(0)), tyList(tyVar(1))), IS("xs", "ys"), tyList(tyPair(tyVar(0), tyVar(1))))(
        envCompMap = CommonComps.standardComps.updated("createPair", CommonComps.createPair(tyFixVar(0), tyFixVar(1))),
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def squareListSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(-3) -> listValue(),
        argList(0) -> listValue(),
        argList(1) -> listValue(1),
        argList(2) -> listValue(1, 4),
        argList(3) -> listValue(1, 4, 9),
        argList(4) -> listValue(1, 4, 9, 16)
      )

      val refComp = CommonComps.squareList

      synthesize("squareList", IS(tyInt), IS("n"), tyList(tyInt))(
        envCompMap = CommonComps.standardComps ++ CommonComps.timesAndDiv,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def fibSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(-1) -> 1,
        argList(0) -> 1,
        argList(1) -> 1,
        argList(2) -> 2,
        argList(3) -> 3,
        argList(4) -> 5,
        argList(5) -> 8,
        argList(6) -> 13
      )

      val refComp = CommonComps.fib

      synthesize("fib", IS(tyInt), IS("n"), tyInt)(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def gcdSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        //        argList(listValue(), listValue()) -> listValue(),
        argList(3, 7) -> 1,
        argList(12, 8) -> 4,
        argList(9, 12) -> 3,
        argList(9, 3) -> 3,
        argList(8, 4) -> 4,
        argList(2, 3) -> 1,
        argList(7, 3) -> 1
      )

      val refComp = CommonComps.gcd

      synthesize("gcd", IS(tyInt, tyInt), IS("a", "b"), tyInt)(
        envCompMap = CommonComps.standardComps ++ Map("mod" -> CommonComps.modulo),
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def modSynthesis() = {
      val examples: IS[(ArgList, TermValue)] =
        for (a <- -3 to 4; b <- -2 to 2) yield {
          argList(a, b) -> CommonComps.modulo.execute(IS(a, b), debug = false)
        }

      val refComp = CommonComps.modulo

      synthesize("mod", IS(tyInt, tyInt), IS("a", "b"), tyInt)(
        envCompMap = CommonComps.standardComps ++ CommonComps.timesAndDiv,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def insertSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        //        argList(listValue(), listValue()) -> listValue(),
        argList(listValue(), 0, 5) -> listValue(5),
        argList(listValue(), 3, 5) -> listValue(5),
        argList(listValue(3), -1, 1) -> listValue(1,3),
        argList(listValue(1, 2, 3), 0, 8) -> listValue(8, 1, 2, 3),
        argList(listValue(1, 2, 3), 1, 8) -> listValue(1, 8, 2, 3),
        argList(listValue(1, 2, 3), 2, 8) -> listValue(1, 2, 8, 3),
        argList(listValue(1, 2, 3), 3, 8) -> listValue(1, 2, 3, 8),
        argList(listValue(1, 2, 3), 4, 8) -> listValue(1, 2, 3, 8)
      )

      val refComp = CommonComps.insert

      synthesize("insert", IS(tyList(tyVar(0)), tyInt, tyVar(0)), IS("xs", "i", "x"), tyList(tyVar(0)))(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def removeDupSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(7)) -> listValue(7),
        argList(listValue(2,3,3,9,9)) -> listValue(2,3,9),
        argList(listValue(3,3,3,9)) -> listValue(3,9),
        argList(listValue(2,3,9)) -> listValue(2,3,9),
        argList(listValue(9,9)) -> listValue(9),
        argList(listValue(3,9)) -> listValue(3,9),
        argList(listValue(9,9,2)) -> listValue(9,2)
      )

      val refComp = CommonComps.removeDup

      synthesize("removeDup", IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def nodesAtLevelSynthesis() = {
      import BinaryTree._

      val args: IS[ArgList] = IS(
        argList(BinaryLeaf, 1),
        argList(BinaryLeaf, 0),
        argList(BinaryLeaf, -1),
        argList(exNode(12), -1),
        argList(exNode(12), 0),
        argList(exNode(12), 1),
        argList(exNode(12), 2),
        argList(BinaryNode(12, exNode(7), exNode(9)), 1),
        argList(BinaryNode(12, exNode(7), exNode(9)), 2),
        argList(BinaryNode(12, BinaryNode(15, exNode(4), BinaryLeaf), exNode(9)), 3),
        argList(BinaryNode(15, BinaryNode(15, exNode(4), BinaryLeaf), exNode(9)), 4)
      )

      val refComp = CommonComps.nodesAtLevel

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize("nodesAtLevel", IS(tyTree(tyVar(0)), tyInt), IS("tree", "level"), tyList(tyVar(0)))(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    type TestCase = () => Option[(Synthesis.SynthesizedComponent, syn.SynthesisState, SynthesisData)]
    val tasks: Seq[TestCase] =
      Seq(
        reverseSynthesis,
        stutterSynthesis,
        cartesianSynthesis,
        squareListSynthesis,
        fibSynthesis,
        insertSynthesis,
        removeDupSynthesis,
        nodesAtLevelSynthesis
      )
    val modTasks = Seq[TestCase](modSynthesis)

    val records = for (task <- tasks) yield {
      val (time, result) = TimeTools.printTimeUsed("single synthesis") {
        TimeTools.measureTime(task())
      }
      SynthesisTyped.printResult(syn, maxExamplesShown = 8)(result)

      val reboots = result.get._3.reboots
      (result.get._1, reboots, time)
    }

    var totalTime: Long = 0
    println("Summery: ")
    val dataToPrint = records.toIndexedSeq.map {
      case (comp, reboots, time) =>
        totalTime += time
        IS(s"  ${comp.name}",
          s"[cost=${comp.cost}]${if(reboots!=0) s"($reboots reboots)" else ""}",
          "%.1fms".format(time/1e6))
    }
    CmdInteract.printTable(dataToPrint, spacing = 2)
    println(s"Total time: %.1fms".format(totalTime / 1e6))

  }


  def main(args: Array[String]): Unit = {
    testSynthesisTyped()
//    testSynthesisUntyped()
//    testImmutableGoal()
  }
}
