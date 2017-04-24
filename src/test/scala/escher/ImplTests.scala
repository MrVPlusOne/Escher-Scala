package escher

import escher.CommonComps.{isZero, standardComps}
import escher.ComponentImpl.recursiveImpl
import escher.DSL._
import escher.Synthesis.ArgList
import org.scalatest.WordSpec


//noinspection RedundantDefaultArgument
class ImplTests extends WordSpec {
  def checkImpl(impl: ComponentImpl, debugExecute: Boolean = false)(testCases: (Seq[TermValue], TermValue)*): Unit ={
    testCases.foreach{
      case (in, out) =>
        assert(impl.execute(in.toIndexedSeq, debugExecute) === out)
    }
  }

  "a sample length implementation" should {

    val lengthImpl = recursiveImpl(
      name = "length",
      argNames = IS("xs"),
      inputTypes = IS(TList of tyVar(0)),
      returnType = tyInt,
      envComps = standardComps,
      body =
        `if`("isEmpty" $ v("xs")) {
          "zero" $ ()
        } {
          "inc" $ ("length" $ ("tail" $ v("xs")))
        },
      argListCompare = ArgList.alphabeticSmaller,
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

  "a non-terminating implementation" should {
    val forever = recursiveImpl(
      name = "forever",
      argNames = IS("x"),
      inputTypes = IS(tyVar(0)),
      returnType = tyVar(0),
      envComps = standardComps,
      body = "forever" $ v("x"),
      argListCompare = ArgList.alphabeticSmaller,
      debug = false
    )
    "return Err" in {
      checkImpl(forever)(
        List(listValue()) -> ValueError,
        List(ValueError) -> ValueError
      )
    }
  }


  "a simple fib implementation" should {
    val fibImpl = recursiveImpl(
      name = "fib",
      argNames = IS("n"),
      inputTypes = IS(tyInt),
      returnType = tyInt,
      envComps = standardComps + isZero,
      body =
        `if`("or"$ ("isZero" $ v("n"), "isZero" $ ("dec"$ v("n")))) {
          "inc"$("zero"$())
        } {
          "plus"$("fib"$("dec" $ v("n")), "fib"$("dec"$("dec"$ v("n"))))
        },
      argListCompare = ArgList.alphabeticSmaller,
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










