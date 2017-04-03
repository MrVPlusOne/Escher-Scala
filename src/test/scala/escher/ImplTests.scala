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
      inputTypes = List(TList of tVar(0)),
      outputType = TInt.of(),
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
      inputTypes = List(TInt.of()),
      outputType = TInt.of(),
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
  "value type checking" should{
    def checkType(termValue: TermValue, ty: Type): Unit ={
      assert(termValue.matchType(ty, 1).get._1 === TypeSubst.empty)
    }

    "pass examples" in {
      checkType(1, TInt.of())
      checkType(true, TBool.of())
      checkType(ValueError, TInt.of())
      checkType(ValueError, TList.of(tVar(0)))
      checkType(listValue(), TList.of(TBool.of()))
      checkType(listValue(), TList.of(tVar(0)))
      checkType(listValue(1,2,3,4), TList.of(TInt.of()))
      checkType(listValue(listValue()), TList.of(TList.of(tVar(0))))

      assert{ 1.matchType(tVar(0), 1).get._1 === TypeSubst(Map(0->TInt.of()))}

      assert{ listValue(listValue(), listValue(true)).matchType(TList of tVar(0),1).get._1 contains TypeSubst(Map(0 -> TList.of(TBool.of())))}

      assert{ listValue(listValue(), listValue(true)).matchType(tVar(0),1).get._1 contains TypeSubst(Map(0 -> (TList of TList.of(TBool.of()))))}

      assert{
        listValue(1,2,true).matchType(TList of tVar(0), 1) === None
      }
    }
  }


}
