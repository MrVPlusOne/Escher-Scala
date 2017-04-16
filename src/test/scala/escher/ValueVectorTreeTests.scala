package escher

import org.scalatest.WordSpec
import DSL._


class ValueVectorTreeTests extends WordSpec{
  "a tree of string" should {
    val tree = new ValueVectorTree[String](4)
    tree.addTerm("A", IS(1,2,3,4))
    tree.addTerm("B", IS(1,2,6,8))



    "has size 2" in {
      assert{
        tree.size === 2
      }
    }

    "find A and B" in {
      assert{
        tree.searchTerms(Map(0->1,1->2,2->3,3->4)).toList === List("A")
      }

      assert{
        tree.searchATerm(Map(0->1,1->2,2->3,3->4)).get === "A"
      }

      assert{
        tree.searchTerms(Map(0 ->1, 1->2, 2->6, 3->8)).toList === List("B")
      }

      assert{
        tree.searchATerm(Map(0 ->1, 1->2, 2->6, 3->8)).get === "B"
      }

      assert{
        tree.searchTerms(Map(0 -> 1, 1 -> 2)).toSet === Set("A","B")
      }

      assert{
        Set("A","B") contains tree.searchATerm(Map(0 -> 1, 1 -> 2)).get
      }
    }

    "correctly turns into List" in {
      tree.elements.toSet === Set(IS(1,2,6,8) -> "B", IS(1,2,3,4) -> "A")
    }
  }
}
