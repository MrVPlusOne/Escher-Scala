package escher

import escher.DSL._
import escher.Synthesis.BufferedOracle
import org.scalatest.WordSpec


class SynthesisTest extends WordSpec{
  import DSL._
  

  "typesForCosts check" in {
    def typesAtCost(cost: Int): Iterator[Type] = cost match {
      case 1 => Iterator(tyInt)
      case 2 => Iterator(tyInt, tyList(tyVar(0)))
      case 3 => Iterator(tyPair(tyInt, tyBool))
    }
    
    def typesForCosts(costs: IS[Int],
                      inputTypes: IS[Type], returnType: Type) = {
      Synthesis.typesForCosts(typesAtCost, costs, inputTypes, returnType)
    }
    

    assert {
      typesForCosts( IS(1, 1), IS(tyInt, tyInt), tyBool).toSet === Set((IS(tyInt, tyInt), tyBool))
    }

    assert {
      typesForCosts( IS(1, 2), IS(tyInt, tyList(tyVar(0))), tyBool).toSet ===
        Set((IS(tyInt, tyList(tyInt)), tyBool), (IS(tyInt, tyList(tyVar(0))), tyBool))
    }

    assert {
      typesForCosts( IS(1, 2), IS(tyInt, tyList(tyVar(0))), tyVar(0)).toSet ===
        Set((IS(tyInt, tyList(tyInt)), tyInt), (IS(tyInt, tyList(tyVar(0))), tyVar(0)))
    }

    assert {
      typesForCosts( IS(3, 1), IS(tyPair(tyVar(0), tyVar(0)), tyVar(0)), tyList(tyVar(1))).toSet === Set()
    }

    assert {
      typesForCosts( IS(3, 1), IS(tyPair(tyVar(0), tyVar(1)), tyVar(0)), tyList(tyVar(2))).toSet ===
        Set((IS(tyPair(tyInt, tyBool), tyInt), tyList(tyVar(0))))
    }

    assert {
      // check nested list
      typesForCosts( IS(2,2), IS(tyVar(0), tyList(tyVar(0))), tyList(tyInt)).toSet ===
        Set(IS(tyList(tyInt), tyList(tyVar(0)))-> tyList(tyInt),
          IS(tyList(tyVar(0)), tyList(tyVar(0))) -> tyList(tyInt))
    }

    assert {
      typesForCosts( IS(2,2), IS(tyVar(0), tyList(tyVar(0))), tyList(tyVar(0))).toSet ===
        Set(IS(tyList(tyInt), tyList(tyVar(0))) -> tyList(tyList(tyInt)),
          IS(tyList(tyVar(0)), tyList(tyVar(0))) -> tyList(tyList(tyVar(0))))
    }
  }

  "ValueMap" should {
    import escher.Synthesis.IndexValueMap._

    def map(xs: (Int, TermValue)*) = Map[Int, TermValue](xs :_*)

    "split correctly" in {
      assert {
        splitValueMap(Map(1 -> 1, 2 -> 2, 3 -> 0, 4 -> 0), IS(0, 1, 2, 3, 4, 5, 6)).get ===
          (map(1 -> true, 2 -> true, 3 -> false, 4 -> false), map(1 -> 1, 2 -> 2), map(3 -> 0, 4 -> 0))
      }

      assert {
        splitValueMap(Map(1 -> 1, 2->0, 3 ->0, 4->4), IS(0,1,2,3,4,5,6)).get ===
          (map(1 -> true, 2 -> false, 3 -> false, 4 -> true), map(1->1, 4->4), map(2->0, 3->0))
      }
    }
  }
}
