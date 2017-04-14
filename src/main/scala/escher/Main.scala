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

  def testSynthesisTyped(): Unit ={
    import escher.SynthesisTyped._
    import DSL._

    val syn = new SynthesisTyped(Config(maxCost = 20, logComponents = false), Console.print)
    import syn._

    def reverseSynthesis() ={
      val examples: IS[(ArgList, TermValue)] = IS(
        argList(listValue()) -> listValue(),
        argList(listValue(2,3,4)) -> listValue(4,3,2)
      )

      val refComp = CommonComps.reverse

      synthesize("reverse", IS(tyList(tyVar(0))), IS("xs"), tyList(tyVar(0)))(
        envCompMap = CommonComps.noTree,
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
        envCompMap = CommonComps.noTree,
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
        envCompMap = CommonComps.noTree.updated("createPair", CommonComps.createPair(tyFixVar(0),tyFixVar(1))),
        compCostFunction = _ => 1,
        examples, oracle = refComp.impl)
    }

    Synthesis.printTypedSynthesisResult(syn){
      cartesianSynthesis()
    }
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
      envCompMap = CommonComps.noTree,
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

  def testTypeTree(): Unit = {
    import TypeHierarchy._
    import DSL._

    val node = new RootNode()
    def insertType(ty: Type) = insertTypeNode(node, new TypeNode(ty))

    insertType(tyInt)
    insertType(tyBool)
    insertType(tyList(tyInt))
    insertType(tyList(tyList(tyInt)))
    insertType(tyList(tyList(tyVar(0))))
    insertType(tyList(tyVar(0)))

    insertType(tyPair(tyVar(0), tyInt))
    insertType(tyPair(tyInt, tyVar(0)))
    insertType(tyPair(tyInt, tyInt))

    node.printTree()

    insertType(tyPair(tyVar(0), tyVar(0)))
    insertType(tyPair(tyBool,tyBool))

    node.printTree()
  }

  def testImmutableGoal(): Unit ={
    import ImmutableGoalGraph._
    import DSL._

    val manager = new GoalManager(
      initGoal = Map(0->0,1->1,2->2),
      _ => None, _ => None,
      3,
      printer = (n, msg) => print("  " * n + msg)
    )

    manager.insertNewTerm(IS(0, 0, 0), "zero"$())
    manager.insertNewTerm(IS(1,1,1), "inc"$("zero"$()))
    manager.insertNewTerm(IS(true,false,false), "isEmpty"$())
    manager.insertNewTerm(IS(ValueError,1,2), "inc"$("P6"$()))

    manager.printState()
  }

  def main(args: Array[String]): Unit = {
    testSynthesisTyped()
//    testSynthesisUntyped()
//    testImmutableGoal()
  }
}
