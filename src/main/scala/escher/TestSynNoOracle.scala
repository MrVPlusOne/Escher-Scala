package escher

import escher.SynNoOracle._
import DSL._
import Synthesis._
import escher.CommonComps.ReducibleCheck


object TestSynNoOracle {
  def main(args: Array[String]): Unit = {


    val syn = new SynNoOracle(
      Config(maxLevel = 15, logLevels = true, logComponents = false, searchSizeFactor = 3),
      Console.print
    )

    import syn._

    case class TestCase(name: String, examples: IndexedSeq[(ArgList, TermValue)],
                        result: () => Option[SynthesizedComponent])

    def synthesizeUsingRef(refComp: ComponentImpl, argNames: IS[String],
                           exampleInputs: IS[ArgList],
                           additionalComps: (Set[ComponentImpl], Map[ComponentImpl, ReducibleCheck]) = (Set(), Map())) = {
      val examples = exampleInputs.map(argList => argList -> refComp.executeEfficient(argList)).sortWith(Synthesis.exampleLt)
      val additionalImpls = additionalComps._1
      val additionalRules = additionalComps._2


      TestCase(refComp.name, examples, () => {
        println(s"Task name: ${refComp.name}")
        Synthesis.showExamples("sorted examples", examples, maxExamplesShown = 50)
        synthesize(refComp.name, refComp.inputTypes, argNames, refComp.returnType)(
          envComps = CommonComps.standardComps ++ additionalImpls, examples, CommonComps.rules_standard ++ additionalRules)
      })
    }

    val lengthSynthesis = {
      synthesizeUsingRef(CommonComps.length, IS("xs"), exampleInputs = IS(
        argList(listValue(2, 3, 4)),
        argList(listValue(1)),
        argList(listValue())
      ))
    }

    val reverseSynthesis = {
      synthesizeUsingRef(CommonComps.reverse, IS("xs"), exampleInputs = IS(
        argList(listValue(2, 3, 4)),
        argList(listValue(2, 3)),
        argList(listValue(1)),
        argList(listValue())
      ))
    }

    val stutterSynthesis = {
      synthesizeUsingRef(CommonComps.stutter, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(5)),
        argList(listValue(5, 6, 3))
      ))
    }

    val containsSynthesis = {
      synthesizeUsingRef(CommonComps.contains, IS("xs", "x"), exampleInputs = IS(
        argList(listValue(1,2,3), 1),
        argList(listValue(1,2,3), 2),
        argList(listValue(1,2,3), 3),
        argList(listValue(1,2,3), 4),
        argList(listValue(1,2,3), -1),
        argList(listValue(1,2), 3),
        argList(listValue(), 1)
      ))
    }

    val insertSynthesis = {
      synthesizeUsingRef(CommonComps.insert, IS("xs", "i", "x"), exampleInputs = IS(
        argList(listValue(), 0, 5),
        argList(listValue(), 3, 5),
        argList(listValue(3), -1, 1),
        argList(listValue(1, 2, 3), 0, 8),
        argList(listValue(1, 2, 3), 1, 8),
        argList(listValue(1, 2, 3), 2, 8),
        argList(listValue(1, 2, 3), 3, 8),
        argList(listValue(1, 2, 3), 4, 8)
      ))
    }

    val fibSynthesis = {
      synthesizeUsingRef(CommonComps.fib, IS("n"), exampleInputs = IS(
        argList(-3),
        argList(0),
        argList(1),
        argList(2),
        argList(3),
        argList(4),
        argList(5),
        argList(6)
      ))
    }

    val squareListSynthesis = {
      synthesizeUsingRef(CommonComps.squareList, IS("n"), exampleInputs = IS(
        argList(-3),
        argList(0),
        argList(1),
        argList(2),
        argList(3),
        argList(4)
      ), additionalComps = (Set(CommonComps.times), CommonComps.rules_timesAndDiv))
    }

    val nodesAtLevelSynthesis = {
      import BinaryTree._

      synthesizeUsingRef(CommonComps.nodesAtLevel, IS("tree", "level"), exampleInputs = IS(
        argList(BinaryLeaf, 1),
        argList(BinaryLeaf, -1),
        argList(singleNode(1), -1),
        argList(singleNode(1), 0),
        argList(singleNode(1), 1),
        argList(singleNode(1), 2),
        argList(BinaryNode(1, singleNode(7), singleNode(9)), 1),
        argList(BinaryNode(1, BinaryNode(15, singleNode(4), BinaryLeaf), singleNode(9)), 3),
        argList(BinaryNode(1, singleNode(2), BinaryNode(3, singleNode(4), BinaryNode(5, singleNode(6), singleNode(7)))), 0),
        argList(BinaryNode(1, singleNode(2), BinaryNode(3, singleNode(4), BinaryNode(5, singleNode(6), singleNode(7)))), 1),
        argList(BinaryNode(1, singleNode(2), BinaryNode(3, singleNode(4), BinaryNode(5, singleNode(6), singleNode(7)))), 2),
        argList(BinaryNode(1, singleNode(2), BinaryNode(3, singleNode(4), BinaryNode(5, singleNode(6), singleNode(7)))), 3)
      ))
    }

    val dropLastSynthesis = {
      synthesizeUsingRef(CommonComps.dropLast, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1,2)),
        argList(listValue(1,2,3)),
        argList(listValue(1,1,1,2,3,2))
      ))
    }

    val evensSynthesis = {
      synthesizeUsingRef(CommonComps.evens, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1,2)),
        argList(listValue(1,2,3,4)),
        argList(listValue(1,2,3,4,5,6))
      ))
    }

    val lastInList = {
      synthesizeUsingRef(CommonComps.lastInList, IS("xs"), exampleInputs = IS(
        argList(listValue(1)),
        argList(listValue(1,2)),
        argList(listValue(1,2,3)),
        argList(listValue(1,3,7,9))
      ))
    }

    val tConcatSynthesis = {
      import BinaryTree._

      synthesizeUsingRef(CommonComps.tConcat, IS("baseTree", "inserted"), exampleInputs = IS(
        argList(BinaryLeaf, BinaryLeaf),
        argList(BinaryLeaf, singleNode(1)),
        argList(singleNode(1), BinaryLeaf),
        argList(singleNode(1), BinaryNode(2, singleNode(3), singleNode(4))),
        argList(BinaryNode(1, singleNode(2), singleNode(3)), BinaryNode(4, singleNode(5), singleNode(6))),
        argList(BinaryNode(1, BinaryLeaf, BinaryNode(2, singleNode(3), singleNode(4))), singleNode(5))
      ))
    }

    val cartesianSynthesis = {
      synthesizeUsingRef(CommonComps.cartesian, IS("xs", "ys"), exampleInputs = IS(
        argList(listValue(), listValue(2, 3, 4)),
        argList(listValue(5), listValue()),
        argList(listValue(5), listValue(7, 8, 9)),
        argList(listValue(2, 3), listValue(4, 5))
      ), additionalComps = (Set(CommonComps.createPair(tyFixVar(0), tyFixVar(1))), Map()))
    }

    val timesSynthesis = {
      synthesizeUsingRef(CommonComps.times, IS("x", "y"), exampleInputs = IS(
        argList(1,0),
        argList(0,5),
        argList(2,7),
        argList(3,8),
        argList(0,8),
        argList(7,5)
      ))
    }

    val sumUnderSynthesis = {
      synthesizeUsingRef(CommonComps.sumUnder, IS("n"), exampleInputs = IS(
        argList(0),
        argList(1),
        argList(2),
        argList(3),
        argList(4)
      ))
    }

    val flattenTreeSynthesis = {
      import BinaryTree._
      synthesizeUsingRef(CommonComps.flattenTree, IS("tree"), exampleInputs = IS(
        argList(BinaryLeaf),
        argList(singleNode(1)),
        argList(BinaryNode(1, singleNode(2), singleNode(3)))
      ))
    }

    val maxInListSynthesis = {
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

    val lastInListSynthesis  = {
      synthesizeUsingRef(CommonComps.lastInList, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1, 2, 3)),
        argList(listValue(1, 6, 7, 11)),
        argList(listValue(10, 25, 7, 9, 18))
      ))
    }

    val shiftLeftSynthesis  = {
      synthesizeUsingRef(CommonComps.shiftLeft, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(1, 2, 3)),
        argList(listValue(1, 6, 7, 11)),
        argList(listValue(10, 25, 7, 9, 18))
      ))
    }

    val compressTraceCompleteSynthesis = {
      synthesizeUsingRef(CommonComps.compress, IS("xs"), exampleInputs = IS(
          argList(listValue()),
          argList(listValue(2)),
          argList(listValue(2, 3, 3, 9, 9)),
          argList(listValue(2, 3, 9)),
          argList(listValue(3, 3, 3, 9)),
          argList(listValue(3, 3, 9)),
          argList(listValue(3, 3, 9, 9)),
          argList(listValue(3, 9)),
          argList(listValue(3, 9, 9)),
          argList(listValue(7)),
          argList(listValue(9)),
          argList(listValue(9, 2)),
          argList(listValue(9, 9)),
          argList(listValue(9, 9, 2))
      ))
    }

    val dedupTraceCompleteSynthesis = {
      synthesizeUsingRef(CommonComps.dedup, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(3,3)),
        argList(listValue(2,3)),
        argList(listValue(1,2,3)),
        argList(listValue(1,2,3,2)),
        argList(listValue(1,1,1,2,3,2)),
        argList(listValue(2,2,2,3,3,3)),
        argList(listValue(1,2,3,2,1))
      ), additionalComps = (Set(CommonComps.contains), Map()))

      def dedupSynthesis(useContains: Boolean)() = {
        val args: IS[ArgList] = IS(
          argList(listValue()),
          argList(listValue(1)),
          argList(listValue(3,3)),
          argList(listValue(2,3)),
          argList(listValue(1,2,3)),
          argList(listValue(1,2,3,2)),
          argList(listValue(1,1,1,2,3,2)),
          argList(listValue(1,2,3,2,1))
        )
      }
    }

    // below are failed tasks


    // this can be synthesized, but quite verbose
    val compressSynthesis = {
      synthesizeUsingRef(CommonComps.compress, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(7)),
        argList(listValue(3, 9)),
        argList(listValue(9, 9)),
        argList(listValue(2, 3, 9)),
        argList(listValue(9, 9, 2)),
        argList(listValue(3, 3, 3, 9)),
        argList(listValue(2, 3, 3, 9, 9))
      ))
    }

    def dedupSynthesis(useContains: Boolean) = {
      synthesizeUsingRef(CommonComps.dedup, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(1)),
        argList(listValue(3,3)),
        argList(listValue(2,3)),
        argList(listValue(1,2,3)),
        argList(listValue(1,2,3,2)),
        argList(listValue(1,1,1,2,3,2)),
        argList(listValue(2,2,2,3,3,3)),
        argList(listValue(1,2,3,2,1))
      ), additionalComps = (if(useContains) Set(CommonComps.contains) else Set(), Map()))
    }

    val sortIntsSynthesis = {
      synthesizeUsingRef(CommonComps.sortInts, IS("xs"), exampleInputs = IS(
        argList(listValue()),
        argList(listValue(9)),
        argList(listValue(12)),
        argList(listValue(4)),
        argList(listValue(9,12)), argList(listValue(12,9)),
        argList(listValue(12,4)), argList(listValue(4,12)),
        argList(listValue(9,4)), argList(listValue(4,9)),
        argList(listValue(9,12,4))
      ))
    }

    val failedTasks = Seq(
      compressSynthesis,
      dedupSynthesis(true),
      sortIntsSynthesis
    )

    val tasks = Seq(
      sortIntsSynthesis,
      reverseSynthesis,
      lengthSynthesis,
      compressTraceCompleteSynthesis,
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

      fibSynthesis,
      sumUnderSynthesis,
      timesSynthesis,

      flattenTreeSynthesis,
      tConcatSynthesis,
      nodesAtLevelSynthesis
    )

    var totalTime: Long = 0
    var totalCost, totalDepth, totalExamples = 0
    val records = for (task <- tasks) yield {
      val (time, result) = TimeTools.printTimeUsed("single synthesis") {
        val (time, result) = TimeTools.measureTime(task.result())
        result match {
          case Some(component) =>
            println("Program Found: ")
            component.print()
            println(s"cost = ${component.cost}")
            totalTime += time
            totalCost += component.cost
            totalDepth += component.depth
            totalExamples += task.examples.length
          case None => println("Failed")
        }
        (time, result)
      }

      result match {
        case Some(comp) =>
          IS(task.name, comp.cost.toString, comp.depth.toString,
            task.examples.length.toString, TimeTools.nanoToMillisString(time))
        case None =>
          IS(task.name, "FAIL", "FAIL", task.examples.length.toString, "FAIL")
      }
    }

    println("Summery: ")
    val dataToPrint = IS("name", "cost", "depth", "examples", "time") +: records.toIndexedSeq :+
      IS("Total", totalCost.toString, totalDepth.toString,
        totalExamples.toString, TimeTools.nanoToSecondString(totalTime))

    CmdInteract.printTable(dataToPrint, spacing = 2, Set(1,2,3,4))
  }
}
