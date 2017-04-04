package escher

import escher.CommonlyUsedComponents.{allMap, recursiveImpl}
import escher.DSL._
import org.scalatest.WordSpec


//noinspection RedundantDefaultArgument
class ImplTests extends WordSpec {
  def checkImpl(impl: ComponentImpl, debugExecute: Boolean = false)(testCases: (List[TermValue], TermValue)*): Unit ={
    testCases.foreach{
      case (in, out) =>
        assert(impl.execute(in, debugExecute) === out)
    }
  }

  "a sample length implementation" should {

    val lengthImpl = recursiveImpl(
      name = "length",
      argNames = List("xs"),
      inputTypes = List(TList of tyVar(0)),
      outputType = tyInt,
      compMap = allMap,
      body =
        `if`("isEmpty" $ v("xs")) {
          "zero" $ ()
        } {
          "inc" $ ("length" $ ("tail" $ v("xs")))
        },
      debug = false
    )

    "behave correctly" in {
      checkImpl(lengthImpl)(
        List(listValue()) -> 0,
        List(listValue(1,2)) -> 2,
        List(listValue(true, false, true, true)) -> 4
      )
    }
  }

  "a simple fib implementation" should {
    val fibImpl = recursiveImpl(
      name = "fib",
      argNames = List("n"),
      inputTypes = List(tyInt),
      outputType = tyInt,
      compMap = allMap,
      body =
        `if`("or"$ ("isZero" $ v("n"), "isZero" $ ("dec"$ v("n")))) {
          "inc"$("zero"$())
        } {
          "plus"$("fib"$("dec" $ v("n")), "fib"$("dec"$("dec"$ v("n"))))
        },
      debug = false
    )
    "behave correctly" in {
      checkImpl(fibImpl)(
        List(ValueInt(0)) -> ValueInt(1),
        List(ValueInt(1)) -> ValueInt(1),
        List(ValueInt(2)) -> ValueInt(2),
        List(ValueInt(3)) -> ValueInt(3),
        List(ValueInt(4)) -> ValueInt(5),
        List(ValueInt(5)) -> ValueInt(8)
      )
    }
  }
}

class TypeCheckingTests extends WordSpec {
  "value type checking" should {
    def checkType(termValue: TermValue, ty: Type): Unit ={
      assert(termValue.matchType(ty, 1).get._1 === TypeSubst.empty)
    }

    "pass examples" in {
      checkType(1, tyInt)
      checkType(true, tyBool)
      checkType(ValueError, tyInt)
      checkType(ValueError, tyList(tyVar(0)))
      checkType(listValue(), tyList(tyBool))
      checkType(listValue(), tyList(tyVar(0)))
      checkType(listValue(1,2,3,4), tyList(tyInt))
      checkType(listValue(listValue()), tyList(tyList(tyVar(0))))

      assert{ 1.matchType(tyVar(0), 1).get._1 === TypeSubst(Map(0->tyInt))}

      assert{ listValue(listValue(), listValue(true)).matchType(TList of tyVar(0),1).get._1 contains TypeSubst(Map(0 -> tyList(tyBool)))}

      assert{ listValue(listValue(), listValue(true)).matchType(tyVar(0),1).get._1 contains TypeSubst(Map(0 -> (TList of tyList(tyBool))))}

      assert{
        listValue(1,2,true).matchType(TList of tyVar(0), 1) === None
      }
    }
  }

  "Type oneWayUnify" should {
    import Type.oneWayUnify
    def checkOneWayUnify(examples: ((Type, Type), Map[Int, Type])*): Unit ={
      examples.foreach{
        case ((t1,t2), map) =>
          assert {oneWayUnify(t1,t2).get === TypeSubst(map)}
      }
    }

    "unify vars to any type" in {
      checkOneWayUnify(
        (tyVar(1), tyInt) -> Map(1 -> tyInt),
        (tyVar(0), tyList(tyInt)) -> Map(0 -> tyList(tyInt)),
        (tyVar(0), tyList(tyFixVar(0))) -> Map(0 -> tyList(tyFixVar(0)))
      )
    }

    "unify list" in {
      checkOneWayUnify(
        (tyList(tyVar(0)), tyList(tyList(tyInt))) -> Map(0 -> tyList(tyInt))
      )

      assert{ oneWayUnify(tyList(tyVar(0)), tyVar(1)) === None }
    }

    "unify map" in {
      checkOneWayUnify(
        (TMap.of(tyVar(0), tyVar(1)), TMap.of(tyInt, tyBool)) -> Map(0 -> tyInt, 1 -> tyBool)
      )

      assert { oneWayUnify(TMap.of(tyVar(0), tyVar(0)), TMap.of(tyInt, tyBool)) === None }
      assert { oneWayUnify(TMap.of(tyVar(0), tyVar(0)), TMap.of(tyFixVar(0), tyBool)) === None }
    }
  }


}








