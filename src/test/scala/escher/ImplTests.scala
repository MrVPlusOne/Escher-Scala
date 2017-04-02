package escher

import escher.CommonlyUsedComponents.{allMap, recursiveImpl}
import escher.DSL._
import org.scalatest.WordSpec

/**
  * Created by weijiayi on 02/04/2017.
  */
class ImplTests extends WordSpec {
  def checkImpl(impl: ComponentImpl)(testCases: (List[TermValue], TermValue)*): Unit ={
    testCases.foreach{
      case (in, out) =>
        assert(impl.execute(in) === out)
    }
  }

  "a sample length implementation" should {
    val p =
      `if`(c("isEmpty", v("xs"))) {
        "zero" $()
      } {
        "inc" $ ("length" $ ("tail" $ v("xs")))
      }

    val lengthImpl = recursiveImpl("length", List("xs"),
      List(TList of tVar(0)), TInt.of(), allMap.apply,
      p
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
