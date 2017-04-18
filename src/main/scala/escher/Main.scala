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


  def testSynthesisUntyped(): Unit ={
    import escher._
    import DSL._
    import SynthesisUntyped._

    val syn = new SynthesisUntyped(Config(maxCost = 10, printComponents = false, printLevels = false), print)
    import syn._

//    val examples: IS[(ArgList, TermValue)] = IS(
//      IS(listValue()) -> 0,
//      IS(listValue(1)) -> 1,
//      IS(listValue(1,2,3)) -> 3
//    )
//    val examples: IS[(ArgList, TermValue)] = IS(
//      IS[TermValue](listValue(), 0, 5) -> listValue(5),
//      IS[TermValue](listValue(1,2,3), 0, 7) -> listValue(7,1,2,3),
//      IS[TermValue](listValue(1,2,3), 1, 8) -> listValue(1,8,2,3),
//      IS[TermValue](listValue(1,2,3), 2, 8) -> listValue(1,2,8,3),
//      IS[TermValue](listValue(1,2,3), 3, 9) -> listValue(1,2,3,9)
//    )

    val examples: IS[(ArgList, TermValue)] = IS(
      argList(listValue()) -> listValue(),
      argList(listValue(1,2,3,4)) -> listValue(4,3,2,1)
    )

    val refComp = CommonComps.reverse

    synthesize("reverse", refComp.inputTypes, IS("xs"), refComp.returnType)(
      envCompMap = CommonComps.standardComps,
      compCostFunction = _ => 1, examples,
      oracle = refComp.impl) match {
      case Some((program, state)) =>
        println(s"------ Synthesis Succeeded! ------")
        println(s"Input-output examples:")
        examples.foreach{case(a,r) =>
          print(ArgList.showArgList(a))
          print(" -> ")
          println(r.show)
        }
        state.print(exampleCount = examples.length)
        println(s"\nProgram found:")
        println{
          program.show
        }
      case _ =>
        println("------- Synthesis Failed. -------")
    }
  }

  def testSynthesisTyped(): Unit ={
    import escher.SynthesisTyped._
    import DSL._

    val syn = new SynthesisTyped(Config(maxCost = 20, logLevels = false, logReboot = true, logComponents = false), Console.print)
    import syn._

    def reverseSynthesis() ={
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(2,3,4)) -> listValue(4,3,2)
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
        //      argList(listValue(true,false)) -> listValue(true,true,false,false),
        argList(listValue(5)) -> listValue(5,5),
        argList(listValue(5,6,3)) -> listValue(5,5,6,6,3,3)
      )

      val refComp = CommonComps.stutter

      synthesize("stutter", IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(
        envCompMap = CommonComps.standardComps,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def cartesianSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        //        argList(listValue(), listValue()) -> listValue(),
        argList(listValue(), listValue(2,3,4)) -> listValue(),
        argList(listValue(5), listValue(7,8,9)) -> listValue((5,7),(5,8),(5,9)),
        argList(listValue(2,3), listValue(4,5)) -> listValue((2,4),(2,5),(3,4),(3,5))
      )

      val refComp = CommonComps.cartesian

      synthesize("cartesian", IS(tyList(tyVar(0)), tyList(tyVar(1))), IS("xs","ys"), tyList(tyPair(tyVar(0), tyVar(1))))(
        envCompMap = CommonComps.standardComps.updated("createPair", CommonComps.createPair(tyFixVar(0),tyFixVar(1))),
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def squareListSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        //        argList(listValue(), listValue()) -> listValue(),
        argList(0) -> listValue(),
        argList(1) -> listValue(1),
        argList(2) -> listValue(1,4),
        argList(3) -> listValue(1,4,9),
        argList(4) -> listValue(1,4,9,16)
      )

      val refComp = CommonComps.squareList

      synthesize("squareList", IS(tyInt), IS("n"), tyList(tyInt))(
        envCompMap = CommonComps.standardComps ++ CommonComps.timesAndDiv,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def fibSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        //        argList(listValue(), listValue()) -> listValue(),
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
        argList(3,7) -> 1,
        argList(12,8) -> 4,
        argList(9,12) -> 3,
        argList(9,3) -> 3,
        argList(8,4) -> 4,
        argList(2,3) -> 1,
        argList(7,3) -> 1
      )

      val refComp = CommonComps.gcd

      synthesize("gcd", IS(tyInt, tyInt), IS("a","b"), tyInt)(
        envCompMap = CommonComps.standardComps ++ Map("mod" -> CommonComps.modulo),
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def modSynthesis() = {
      val examples: IS[(ArgList, TermValue)] =
        for(a <- -3 to 4; b <- -2 to 2) yield {
          argList(a,b) -> CommonComps.modulo.execute(IS(a,b), debug = false)
        }

      val refComp = CommonComps.modulo

      synthesize("mod", IS(tyInt, tyInt), IS("a","b"), tyInt)(
        envCompMap = CommonComps.standardComps ++ CommonComps.timesAndDiv,
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    def insertSynthesis() = {
      val examples: IS[(ArgList, TermValue)] = IS(
        //        argList(listValue(), listValue()) -> listValue(),
        argList(listValue(), 0, 5) -> listValue(5),
        argList(listValue(), 3, 5) -> listValue(5),
        argList(listValue(1,2,3), 0, 8) -> listValue(8,1,2,3),
        argList(listValue(1,2,3), 1, 8) -> listValue(1,8,2,3),
        argList(listValue(1,2,3), 2, 8) -> listValue(1,2,8,3),
        argList(listValue(1,2,3), 3, 8) -> listValue(1,2,3,8),
        argList(listValue(1,2,3), 4, 8) -> listValue(1,2,3,8)
      )

      val refComp = CommonComps.insert

      synthesize("insert", IS(tyList(tyVar(0)), tyInt, tyVar(0)), IS("xs", "i", "x"), tyList(tyVar(0)))(
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
        insertSynthesis
      )
    val modTasks = Seq[TestCase](modSynthesis)

    TimeTools.printTimeUsed(s"benchmark total time for ${tasks.length} tasks") {
      val records = for (task <- tasks) yield {
        val (time, result) = TimeTools.printTimeUsed("single synthesis") {
          TimeTools.measureTime(task())
        }
        SynthesisTyped.printResult(syn)(result)

        result.get._1.name -> time
      }

      println("Summery: ")
      records.foreach{
        case (taskName, time) =>
          println(s"  $taskName: %.1fms".format(time/1e6))
      }
    }
  }


  def main(args: Array[String]): Unit = {
    testSynthesisTyped()
//    testSynthesisUntyped()
//    testImmutableGoal()
  }
}
