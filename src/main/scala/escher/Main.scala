package escher

/**
  * Created by weijiayi on 04/04/2017.
  */
object Main {
  def main(args: Array[String]): Unit = {
    import escher._
    import DSL._

    import Synthesis._
    val IS = IndexedSeq

    synthesize("length", IS(TList of TInt.of()),
      IS("xs"), TInt.of()
    )(CommonlyUsedComponents.allMap,
      IS(IS(listValue()), IS(listValue(2)), IS(listValue(1,2))),
      IS(0,1,2)
    )
  }
}
