package escher

import escher.CommonComps.ReducibleCheck
import escher.Synthesis.{ArgList, SynthesizedComponent}


/**
  * Run TypedEscher on a benchmark suit.
  */
object RunTypedEscher {

  case class TestCase(name: String, examples: IndexedSeq[(ArgList, TermValue)],
                      result: () => Option[SynthesizedComponent])


  def testSynthesisTyped(): Unit = {
    import escher.TypedEscherSynthesizer._
    import DSL._

    val syn = new TypedEscherSynthesizer(
      Config(maxCost = 20, logLevels = false, logReboot = false, logComponents = false, searchSizeFactor = 3),
      Console.print
    )

    import syn._


    def synthesizeUsingRef(refComp: ComponentImpl, argNames: IS[String],
                           exampleInputs: IS[ArgList],
                           additionalComps: Set[ComponentImpl] = Set()) = {
      val args: IS[ArgList] = exampleInputs

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, refComp.inputTypes, argNames, refComp.returnType)(envComps = CommonComps.standardComps ++ additionalComps, examples, oracle = refComp.impl)
    }

    def reverseSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(1,2)) -> listValue(2,1),
        argList(listValue(1,2,3)) -> listValue(3,2,1)
      )

      val refComp = CommonComps.reverse

      synthesize(refComp.name, IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def stutterSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(5)) -> listValue(5, 5),
        argList(listValue(5, 6, 3)) -> listValue(5, 5, 6, 6, 3, 3)
      )

      val refComp = CommonComps.stutter

      synthesize(refComp.name, IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def cartesianSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue(), listValue(2, 3, 4)) -> listValue(),
        argList(listValue(5), listValue()) -> listValue(),
        argList(listValue(5), listValue(7, 8, 9)) -> listValue((5, 7), (5, 8), (5, 9)),
        argList(listValue(2, 3), listValue(4, 5)) -> listValue((2, 4), (2, 5), (3, 4), (3, 5))
      )

      val refComp = CommonComps.cartesian

      synthesize("cartesian", IS(tyList(tyVar(0)), tyList(tyVar(1))), IS("xs", "ys"), tyList(tyPair(tyVar(0), tyVar(1))))(envComps = CommonComps.standardComps + CommonComps.createPair(tyFixVar(0), tyFixVar(1)), examples, oracle = refComp.impl)
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

      synthesize("squareList", IS(tyInt), IS("n"), tyList(tyInt))(envComps = CommonComps.standardComps ++ CommonComps.timesAndDiv, examples, oracle = refComp.impl)
    }

    def fibSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(-3) -> 1,
        argList(0) -> 1,
        argList(1) -> 1,
        argList(2) -> 2,
        argList(3) -> 3,
        argList(4) -> 5,
        argList(5) -> 8,
        argList(6) -> 13
      )

      val refComp = CommonComps.fib

      synthesize("fib", IS(tyInt), IS("n"), tyInt)(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
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

      synthesize("gcd", IS(tyInt, tyInt), IS("a", "b"), tyInt)(envComps = CommonComps.standardComps ++ CommonComps.timesAndDiv + CommonComps.modulo, examples, oracle = refComp.impl)
    }

    def modSynthesis() = {
      val args: IS[(ArgList)] = IS(
        argList(0, 1),
        argList(0, 5),
        argList(7, 1),
        argList(7, 2),
        argList(7, 3),
        argList(15, 8),
        argList(16, 9),
        argList(14, -2),
        argList(14, 4)
      )

      val refComp = CommonComps.modulo
      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize("mod", IS(tyInt, tyInt), IS("a", "b"), tyInt)(envComps = CommonComps.standardComps ++ CommonComps.timesAndDiv, examples, oracle = refComp.impl)
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

      synthesize("insert", IS(tyList(tyVar(0)), tyInt, tyVar(0)), IS("xs", "i", "x"), tyList(tyVar(0)))(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def compressSynthesis() = {
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

      val refComp = CommonComps.compress

      synthesize("compress", IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def nodesAtLevelSynthesis() = {
      import BinaryTree._

      val args: IS[ArgList] = IS(
        argList(BinaryLeaf, 1),
        argList(BinaryLeaf, 0),
        argList(BinaryLeaf, -1),
        argList(singleNode(12), -1),
        argList(singleNode(12), 0),
        argList(singleNode(12), 1),
        argList(singleNode(12), 2),
        argList(BinaryNode(12, singleNode(7), singleNode(9)), 1),
        argList(BinaryNode(12, singleNode(7), singleNode(9)), 2),
        argList(BinaryNode(12, BinaryNode(15, singleNode(4), BinaryLeaf), singleNode(9)), 3),
        argList(BinaryNode(15, BinaryNode(15, singleNode(4), BinaryLeaf), singleNode(9)), 4)
      )

      val refComp = CommonComps.nodesAtLevel

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, IS(tyTree(tyVar(0)), tyInt), IS("tree", "level"), tyList(tyVar(0)))(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

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
      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, refComp.inputTypes, IS("xs", "x"), refComp.returnType)(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def dedupSynthesis(useContains: Boolean)() = {
      val args: IS[ArgList] = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(3,3)),
        argList(listValue(2,3)),
        argList(listValue(1,2,3)),
        argList(listValue(1,2,3,2)),
        argList(listValue(1,1,1,2,3,2)),
        argList(listValue(2,2,2,3,3,3)),
        argList(listValue(1,2,3,2,1))
      )

      val refComp = CommonComps.dedup

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      val envCompMap =
        if(useContains) CommonComps.standardComps + CommonComps.contains
        else CommonComps.standardComps

      synthesize(refComp.name, refComp.inputTypes, IS("xs"), refComp.returnType)(envCompMap, examples, oracle = refComp.impl)
    }

    def dropLastSynthesis() = {
      val args: IS[ArgList] = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1,2)),
        argList(listValue(1,2,3)),
        argList(listValue(1,1,1,2,3,2))
      )

      val refComp = CommonComps.dropLast

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, refComp.inputTypes, IS("xs"), refComp.returnType)(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def flattenTreeSynthesis() = {
      import BinaryTree._
      val args = IS(
        argList(BinaryLeaf),
        argList(singleNode(1)),
        argList(BinaryNode(1, singleNode(2), singleNode(3)))
      )
      val refComp = CommonComps.flattenTree

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, refComp.inputTypes, IS("tree"), refComp.returnType)(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def evensSynthesis() = {
      val args: IS[ArgList] = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1,2)),
        argList(listValue(1,2,3,4)),
        argList(listValue(1,2,3,4,5,6))
      )

      val refComp = CommonComps.evens
      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, refComp.inputTypes, IS("xs"), refComp.returnType)(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def tConcatSynthesis() = {
      import BinaryTree._

      val args: IS[ArgList] = IS(
        argList(BinaryLeaf, BinaryLeaf),
        argList(BinaryLeaf, singleNode(1)),
        argList(singleNode(1), BinaryLeaf),
        argList(singleNode(1), BinaryNode(2, singleNode(3), singleNode(4))),
        argList(BinaryNode(1, singleNode(2), singleNode(3)), BinaryNode(4, singleNode(5), singleNode(6))),
        argList(BinaryNode(1, BinaryLeaf, BinaryNode(2, singleNode(3), singleNode(4))), singleNode(5))
      )

      val refComp = CommonComps.tConcat

      val examples = args.map(argList => argList -> refComp.execute(argList, debug = false))

      synthesize(refComp.name, refComp.inputTypes, IS("baseTree", "inserted"), refComp.returnType)(envComps = CommonComps.standardComps, examples, oracle = refComp.impl)
    }

    def sortIntsSynthesis() = {
      synthesizeUsingRef(CommonComps.sortInts, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(5,6,7)),
        argList(listValue(7,6,5)),
        argList(listValue(5,7,6)),
        argList(listValue(9,12,4,3))
      ))
    }

    def lengthSynthesis() = {
      synthesizeUsingRef(CommonComps.length, IS("xs"), exampleInputs = IS(
        argList(listValue(2, 3, 4)),
        argList(listValue(1)),
        argList(listValue())
      ))
    }

    def sumUnderSynthesis() = {
      synthesizeUsingRef(CommonComps.sumUnder, IS("n"), exampleInputs = IS(
        argList(0),
        argList(1),
        argList(2),
        argList(3),
        argList(4)
      ))
    }

    def timesSynthesis() = {
      synthesizeUsingRef(CommonComps.times, IS("x", "y"), exampleInputs = IS(
        argList(1,0),
        argList(0,5),
        argList(2,7),
        argList(3,8),
        argList(0,8),
        argList(7,5)
      ))
    }

    def lastInListSynthesis()  = {
      synthesizeUsingRef(CommonComps.lastInList, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1, 2, 3)),
        argList(listValue(1, 6, 7, 11)),
        argList(listValue(10, 25, 7, 9, 18))
      ))
    }

    def shiftLeftSynthesis()  = {
      synthesizeUsingRef(CommonComps.shiftLeft, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1, 2, 3)),
        argList(listValue(1, 6, 7, 11)),
        argList(listValue(10, 25, 7, 9, 18))
      ))
    }

    def maxInListSynthesis() = {
      synthesizeUsingRef(CommonComps.maxInList, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(3)),
        argList(listValue(0, 2, 1)),
        argList(listValue(1, 6, 2, 5)),
        argList(listValue(1, 6, 7, 5)),
        argList(listValue(10, 25, 7, 9, 18)),
        argList(listValue(100, 25, 7, 9, 18))
      ))
    }

    type TestCase = () => Option[(Synthesis.SynthesizedComponent, syn.SynthesisState, SynthesisData)]
    val tasks: Seq[TestCase] =
      Seq(
        reverseSynthesis,
        lengthSynthesis,
        compressSynthesis,
        stutterSynthesis,
        squareListSynthesis,
        insertSynthesis,
        containsSynthesis,
        lastInListSynthesis,
        shiftLeftSynthesis,
        maxInListSynthesis,
        dropLastSynthesis,
        evensSynthesis,
        cartesianSynthesis,
        dedupSynthesis(useContains = true),

        fibSynthesis,
        sumUnderSynthesis,
        timesSynthesis,

        flattenTreeSynthesis,
        tConcatSynthesis,
        nodesAtLevelSynthesis
      )
    val slowTasks = Seq[TestCase](modSynthesis, sortIntsSynthesis, dedupSynthesis(useContains = false))

    val records = for (task <- tasks ++ slowTasks) yield {
      val (time, result) = TimeTools.printTimeUsed("single synthesis") {
        TimeTools.measureTime(task())
      }
      TypedEscherSynthesizer.printResult(syn, maxExamplesShown = 8)(result)

      val (comp,state,synData) = result.get
      val examples = (state.examples.length, synData.oracleBuffer.length)
      (comp, examples, synData.reboots, time)
    }

    var totalTime: Long = 0
    var totalCost, totalDepth, totalExample1, totalExample2, totalReboots = 0
    println("Summery: ")
    val dataToPrint = IS("  name", "cost", "depth", "examples", "reboots", "time") +: records.toIndexedSeq.map {
      case (comp, examples, reboots, time) =>
        totalTime += time
        totalCost += comp.cost
        totalDepth += comp.depth
        totalExample1 += examples._1
        totalExample2 += examples._2
        totalReboots += reboots
        IS(s"  ${comp.signature.name}",
          comp.cost.toString,
          comp.depth.toString,
          s"${examples._1}/${examples._2}",
          if(reboots==0) "None" else reboots.toString,
          TimeTools.nanoToMillisString(time))
    } :+ IS("  Total", totalCost.toString, totalDepth.toString,
      s"$totalExample1/$totalExample2", totalReboots.toString, TimeTools.nanoToSecondString(totalTime))

    CmdInteract.printTable(dataToPrint, spacing = 2, Set(1,2,3,4,5))
  }


  def main(args: Array[String]): Unit = {
    testSynthesisTyped()
  }
}
