package escher

import escher.Synthesis.{ValueMap, ValueVector}
import collection.mutable
import BatchGoalSearch._

object BatchGoalSearch{
  sealed trait SearchResult

  case class NotFoundUnderCost(cost: Int) extends SearchResult

  case class FoundAtCost(cost: Int, term: Term) extends SearchResult
}


class BatchGoalSearch(
                      termOfCostAndVM: (Int, ValueMap) => Option[Term],
                      termsOfCost: Int => Iterable[(ValueVector,Term)],
                      boolOfVM: ValueMap => Option[Term]) {

  private val buffer = mutable.Map[ValueMap, SearchResult]()

  def search(cost: Int, currentGoal: ValueMap): Option[Term] = {
    buffer.get(currentGoal).foreach{
      case FoundAtCost(c, term) if c <= cost => return Some(term)
      case NotFoundUnderCost(c) if c >= cost => return None
      case _ =>
    }

    termOfCostAndVM(cost, currentGoal) match {
      case Some(term) =>
        buffer(currentGoal) = FoundAtCost(cost, term)
        Some(term)
      case None =>
        for(
          c <- 1 until cost;
          (thenVec ,tThen) <- termsOfCost(c);
          (vm1,_,vm3) <- ValueMap.splitValueMap(currentGoal, thenVec);
          tCond <- boolOfVM(vm1);
          tElse <- search(cost-c, vm3)
        ){
          import DSL._
          val t = `if`(tCond)(tThen)(tElse)
          buffer(currentGoal) = FoundAtCost(cost, t)
          return Some(t)
        }
        buffer(currentGoal) = NotFoundUnderCost(cost)
        None
    }
  }
}
