package escher

import escher.Synthesis.{ValueMap, ValueVector}


class BatchGoalSearch(
                      termOfCostAndVM: (Int, ValueMap) => Option[Term],
                      termsOfCost: Int => Iterator[(ValueVector,Term)],
                      boolOfVM: ValueMap => Option[Term]) {

  def search(cost: Int, currentGoal: ValueMap): Option[Term] = {
    termOfCostAndVM(cost, currentGoal) match {
      case Some(term) => Some(term)
      case None =>
        for(
          c <- 1 until cost;
          (thenVec ,tThen) <- termsOfCost(c);
          (vm1,_,vm3) <- ValueMap.splitValueMap(currentGoal, thenVec);
          tCond <- boolOfVM(vm1);
          tElse <- search(cost-c, vm3)
        ){
          import DSL._
          return Some(`if`(tCond)(tThen)(tElse))
        }
        None
    }
  }
}
