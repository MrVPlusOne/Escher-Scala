package escher

import escher.Synthesis.{IndexValueMap, ValueVector, splitGoal}

import collection.mutable
import BatchGoalSearch._

object BatchGoalSearch{
  sealed trait SearchResult{
    def foreach(f: (Int, Term) => Unit): Unit = this match {
      case FoundAtCost(cost, term) => f(cost, term)
      case NotFoundUnderCost(_) =>
    }
  }

  case class NotFoundUnderCost(cost: Int) extends SearchResult

  case class FoundAtCost(cost: Int, term: Term) extends SearchResult

  def emptyBuffer(): mutable.Map[Set[Int], SearchResult] = mutable.Map[Set[Int], SearchResult]()

  def maxSatConditions(vm: IndexValueMap,
                       boolOfVM: IndexValueMap => Option[(Int,Term)]): Option[((Int, Term), List[Int])] = {
    var keyList = vm.keys.filter(i => vm(i) == ValueBool(true)).toList.sorted.reverse
    var vm1 = vm
    while(keyList.nonEmpty){
      boolOfVM(vm1).foreach(x => return Some(x -> keyList))
      vm1 = vm1.updated(keyList.head, ValueBool(false))
      keyList = keyList.tail
    }
    None
  }
}


class BatchGoalSearch(maxCompCost: Int,
                      termOfCostAndVM: (Int, IndexValueMap) => Option[Term],
                      termsOfCost: Int => Iterable[(ValueVector,Term)],
                      boolTermsOfCost: Int => Iterable[(ValueVector, Term)],
                      boolOfVM: IndexValueMap => Option[(Int,Term)]) {
  private val buffer: mutable.Map[Set[Int], SearchResult] = mutable.Map()


  def searchThenFirst(cost: Int, currentGoal: IndexValueMap): Option[(Int,Term)] = {
    val keySet = currentGoal.keySet
    buffer.get(keySet).foreach {
      case FoundAtCost(c, term) if c <= cost =>
        return Some(c -> term)
      case NotFoundUnderCost(c) if c >= cost =>
        return None
      case _ =>
    }

    def buffered(searchResult: SearchResult): Option[(Int,Term)] = {
      buffer(keySet) = searchResult
      searchResult match {
        case NotFoundUnderCost(_) => None
        case FoundAtCost(c, term) => Some(c -> term)
      }
    }

    val maxCost = math.min(maxCompCost, cost)
    for (c <- 1 to maxCost) {
      termOfCostAndVM(c, currentGoal).foreach { term =>
        return buffered(FoundAtCost(c, term))
      }
    }

    val ifCost = 1

    var minCostCandidate: Option[(Int, Term)] = None
    //noinspection ReplaceToWithUntil
    for (
      cThen <- 1 to maxCost - 1 - ifCost; // save one for cCond
      (thenVec, tThen) <- termsOfCost(cThen);
      (vm, _, _) <- IndexValueMap.splitValueMap(currentGoal, thenVec);
      ((cCond, tCond), trueKeys) <- maxSatConditions(vm, boolOfVM);
      elseGoal = currentGoal -- trueKeys;
      costSoFar = cThen + cCond + ifCost;
      maxCostForElse = math.min(cost, minCostCandidate.map(_._1).getOrElse(Int.MaxValue) - 1) - costSoFar;
      (cElse, tElse) <- searchThenFirst(maxCostForElse, elseGoal)
    ) {
      import DSL._

      val totalCost = cElse + costSoFar
      val t = `if`(tCond)(tThen)(tElse)

      minCostCandidate = Some(totalCost -> t)
    }

    minCostCandidate match{
      case Some((c, t)) => buffered(FoundAtCost(c,t))
      case None => buffered(NotFoundUnderCost(cost))
    }
  }

  /** search conditions before thenBranches */
  def searchCondFirst(cost: Int, currentGoal: IndexValueMap): Option[(Int,Term)] = {
    val keySet = currentGoal.keySet
    buffer.get(keySet).foreach {
      case FoundAtCost(c, term) if c <= cost =>
        return Some(c -> term)
      case NotFoundUnderCost(c) if c >= cost =>
        return None
      case _ =>
    }

    def buffered(searchResult: SearchResult): Option[(Int,Term)] = {
      buffer(keySet) = searchResult
      searchResult match {
        case NotFoundUnderCost(_) => None
        case FoundAtCost(c, term) => Some(c -> term)
      }
    }

    val maxCost = math.min(maxCompCost, cost)
    for (c <- 1 to maxCost) {
      termOfCostAndVM(c, currentGoal).foreach { term =>
        return buffered(FoundAtCost(c, term))
      }
    }

    val ifCost = 1

    var minCostCandidate: Option[(Int, Term)] = None
    //noinspection ReplaceToWithUntil
    for {
      cCond <- 1 to math.min(maxCompCost, cost - ifCost - 2)
      (condVec, tCond) <- boolTermsOfCost(cCond)
      (thenGoal, elseGoal) <- splitGoal(condVec, currentGoal)
    } {
      def thenCandidate: Option[(Int,Term)] = {
        for(cThen <- 1 to math.min(maxCompCost, cost - ifCost - cCond - 1)) {
          termsOfCost(cThen).foreach {
            case (vv, term) =>
              if (IndexValueMap.matchVector(thenGoal, vv))
                return Some(cThen, term)
          }
        }
        None
      }

      for{
        (cThen, tThen) <- thenCandidate
        costSoFar = cThen + cCond + ifCost
        maxCostForElse = math.min(cost, minCostCandidate.map(_._1).getOrElse(Int.MaxValue) - 1) - costSoFar
        (cElse, tElse) <- searchCondFirst(maxCostForElse, elseGoal)
      } {
        import DSL._

        val totalCost = cElse + costSoFar
        val t = `if`(tCond)(tThen)(tElse)

        minCostCandidate = Some(totalCost -> t)
      }
    }

    minCostCandidate match{
      case Some((c, t)) => buffered(FoundAtCost(c,t))
      case None => buffered(NotFoundUnderCost(cost))
    }
  }

}
