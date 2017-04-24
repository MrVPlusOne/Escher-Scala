package escher

import escher.Synthesis.{ArgList, ExtendedValueVec, IndexValueMap, ValueVector}

import collection.mutable
import BatchGoalSearch._

class DynamicGoalSearch( assembleRecProgram: Term => ComponentImpl,
                         maxCompCost: Int,
                         inputVector: IS[ArgList],
                         termOfCostAndVM: (Int, IndexValueMap) => Option[Term],
                         termsOfCost: Int => Iterable[(ValueVector,Term)],
                         boolOfVM: IndexValueMap => Option[(Int,Term)]
                       ) {
//  private val buffer: mutable.Map[Set[Int], SearchResult] = mutable.Map()


  def searchMin(cost: Int, currentGoal: IndexValueMap,
                recTermsOfReturnType: IS[Seq[(Term, ExtendedValueVec)]],
                fillTermToHole: Term => Term,
                isFirstBranch: Boolean): Option[(Int,Term)] = {
    val keySet = currentGoal.keySet
//    buffer.get(keySet).foreach {
//      case FoundAtCost(c, term) if c <= cost =>
//        return Some(c -> term)
//      case NotFoundUnderCost(c) if c >= cost =>
//        return None
//      case _ =>
//    }

    def buffered(searchResult: SearchResult): Option[(Int,Term)] = {
//      buffer(keySet) = searchResult
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
      import ExtendedValueVec.MatchResult._
      if(!isFirstBranch){
        recTermsOfReturnType(c-1).foreach{
          case (term, vv) =>
            ExtendedValueVec.matchWithIndexValueMap(vv, currentGoal) match {
              case ExactMatch => return buffered(FoundAtCost(c, term))
              case PossibleMatch(leftToCheck) =>
                val p = assembleRecProgram(fillTermToHole(term))
                val passCheck = leftToCheck.forall{ case (i, desired) =>
                  p.executeEfficient(inputVector(i)) == desired
                }
                if(passCheck)
                  return buffered(FoundAtCost(c, term))
              case NotMatch =>
            }
        }
      }
    }

    val ifCost = 1

    var minCostCandidate: Option[(Int, Term)] = None
    //noinspection ReplaceToWithUntil
    for (
      cThen <- 1 to maxCost - 1 - ifCost; // save one for cCond
      (thenVec, tThen) <- termsOfCost(cThen);
      (vm, _, _) <- IndexValueMap.splitValueMap(currentGoal, thenVec);
      ((cCond, tCond), trueKeys) <- maxSatConditions(vm, boolOfVM)
    ) {
      val elseGoal = currentGoal -- trueKeys
      val newRecTermsOfCost = recTermsOfReturnType.map(_.map {
        case (term, vv) =>
          val newVV = vv.indices.map { i =>
            vv(i) match {
              case ValueUnknown =>
                if (trueKeys contains i)
                  thenVec(i)
                else ValueUnknown
              case tv: TermValue => tv
            }
          }
          term -> newVV
      })

      import DSL._
      def assembleTerm(tElse: Term): Term = {
        `if`(tCond)(tThen)(tElse)
      }
      for (
        (cElse, tElse) <- searchMin(cost - cThen - cCond - ifCost, elseGoal, newRecTermsOfCost,
          assembleTerm, isFirstBranch = false)
      ) {

        val totalCost = cElse + cThen + cCond + ifCost
        val t = `if`(tCond)(tThen)(tElse)

        minCostCandidate match {
          case Some((c, _)) =>
            if (c > totalCost)
              minCostCandidate = Some(totalCost -> t)
          case None =>
            minCostCandidate = Some(totalCost -> t)
        }
      }
    }
    minCostCandidate match {
      case Some((c, t)) => buffered(FoundAtCost(c, t))
      case None => buffered(NotFoundUnderCost(cost))
    }
  }

}

