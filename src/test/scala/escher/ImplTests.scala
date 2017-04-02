package escher

import escher.CommonlyUsedComponents.{allMap, recursiveImpl}
import escher.DSL._
import org.scalatest.WordSpec


class ImplTests extends WordSpec {
  def checkImpl(impl: ComponentImpl)(testCases: (List[TermValue], TermValue)*): Unit ={
    testCases.foreach{
      case (in, out) =>
        assert(impl.execute(in) === out)
    }
  }

  "a sample length implementation" should {

    val lengthImpl = recursiveImpl(
      name = "length",
      argNames = List("xs"),
      inputTypes = List(TList of tVar(0)),
      outputType = TInt.of(),
      compMap = allMap.apply,
      body =
        `if`("isEmpty" $ v("xs")) {
          "zero" $ ()
        } {
          "inc" $ ("length" $ ("tail" $ v("xs")))
        }
    )

    "behave correctly" in {
      checkImpl(lengthImpl)(
        List(listValue()) -> 0,
        List(listValue(1,2)) -> 2,
        List(listValue(true, false, true, true)) -> 4
      )
    }
  }

}
