package escher


import escher.DSL._
import org.scalatest.WordSpec

/**
  * Created by weijiayi on 05/04/2017.
  */
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

  "Type unify" should {
    import Type.unify
    def checkUnify(examples: ((Type, Type), Map[Int, Type])*): Unit ={
      examples.foreach{
        case ((t1,t2), map) =>
          assert {unify(t1,t2).get === TypeSubst(map)}
      }
    }

    "unify vars to any type" in {
      checkUnify(
        (tyVar(1), tyInt) -> Map(1 -> tyInt),
        (tyVar(0), tyList(tyInt)) -> Map(0 -> tyList(tyInt)),
        (tyVar(0), tyList(tyFixVar(0))) -> Map(0 -> tyList(tyFixVar(0))),
        (tyInt, tyVar(0)) -> Map(0 -> tyInt)
      )
    }

    "unify list" in {
      checkUnify(
        (tyList(tyVar(0)), tyList(tyList(tyInt))) -> Map(0 -> tyList(tyInt)),
        (tyList(tyVar(0)), tyVar(1)) -> Map(1 -> tyList(tyVar(0))),
        (tyList(tyInt), tyList(tyVar(0))) -> Map(0 -> tyInt)
      )

    }

    "unify map" in {
      checkUnify(
        (TMap.of(tyVar(0), tyVar(1)), TMap.of(tyInt, tyBool)) -> Map(0 -> tyInt, 1 -> tyBool),
        (TMap.of(tyVar(0), tyVar(0)), TMap.of(tyBool, tyBool)) -> Map(0 -> tyBool),
        (tyMap(tyList(tyVar(1)), tyVar(2)), tyVar(0)) -> Map(0 -> tyMap(tyList(tyVar(1)), tyVar(2)))
      )

      assert { unify(TMap.of(tyVar(0), tyVar(0)), TMap.of(tyInt, tyBool)) === None }
      assert { unify(TMap.of(tyVar(0), tyVar(0)), TMap.of(tyFixVar(0), tyBool)) === None }
    }
  }

  "alpha-normalization" should{
    def normalizeThenEqual(t1: Type, t2: Type): Unit ={
      assert{ Type.alphaNormalForm(t1) === Type.alphaNormalForm(t2)}
    }

    def normalizeThenDiffer(t1: Type, t2: Type): Unit ={
      assert{ Type.alphaNormalForm(t1) !== Type.alphaNormalForm(t2)}
    }

    "pass examples" in {
      normalizeThenEqual(tyInt, tyInt)
      normalizeThenDiffer(tyInt, tyBool)

      normalizeThenEqual(tyVar(0), tyVar(1))
      normalizeThenDiffer(tyVar(0), tyList(tyVar(0)))

      normalizeThenEqual(tyMap(tyVar(1),tyVar(2)), tyMap(tyVar(3), tyVar(1)))
      normalizeThenDiffer(tyMap(tyVar(1), tyVar(2)), tyMap(tyVar(0), tyVar(0)))
    }
  }

  "instanceOf" should{

    "pass examples" in {
      assert{ tyInt.instanceOf(tyVar(1)) }
      assert{ tyList(tyInt).instanceOf(tyVar(1)) }
      assert{ tyInt.instanceOf(tyInt) }
      assert{ tyVar(1).instanceOf(tyVar(2)) }
      assert{ tyMap(tyVar(1), tyVar(1)) instanceOf tyMap(tyVar(3),tyVar(4))}
      assert{ tyMap(tyVar(1), tyVar(1)) instanceOf tyMap(tyVar(1),tyVar(0))}
      assert{ !tyMap(tyVar(0), tyVar(1)).instanceOf(tyMap(tyVar(1),tyVar(1)))}
      assert { tyMap(tyList(tyVar(1)), tyVar(0)) instanceOf tyVar(0) }
      assert{ tyList(tyInt).instanceOf(tyList(tyVar(1))) }
      assert { tyList(tyList(tyInt)) instanceOf tyList(tyVar(1))}
    }

  }


}
