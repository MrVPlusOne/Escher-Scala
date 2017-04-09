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

  def testSynthesis(): Unit ={
    import escher.SynthesisTyped._
    import DSL._

    new SynthesisTyped(Config(maxCost = 20, logComponents = false), Console.print).synthesize("length", IS(tyList(tyFixVar(0))), IS("xs"), tyInt)(envCompMap = CommonComps.noTree ++ CommonComps.treeComps, compCostFunction = _ => 1, IS(IS(listValue()), IS(listValue(2)), IS(listValue(1,2))), IS(0,1,2), oracle = CommonComps.length.impl)
  }

  def testSynthesisUntyped(): Unit ={
    import escher._
    import DSL._

    import SynthesisUntyped._
    val IS = IndexedSeq

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
//    testSynthesis()
    testSynthesisUntyped()
//    testImmutableGoal()
  }
}
