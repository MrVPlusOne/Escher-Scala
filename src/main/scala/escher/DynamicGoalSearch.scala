package escher

import escher.Synthesis._

import collection.mutable
import BatchGoalSearch._
import escher.DynamicGoalSearch.ExecuteHoleException
import escher.SynNoOracle.ExtendedCompImpl

class DynamicGoalSearch(
                         maxCompCost: Int,
                         signature: ComponentSignature,
                         envComps: Set[ComponentImpl],
                         argListCompare: (ArgList, ArgList) => Boolean,
                         inputVector: IS[ArgList],
                         termOfCostAndVM: (Int, IndexValueMap) => Option[Term],
                         termsOfCost: Int => Iterable[(ValueVector,Term)],
                         boolOfVM: IndexValueMap => Option[(Int,Term)]
                       ) {
//  private val buffer: mutable.Map[Set[Int], SearchResult] = mutable.Map()

  val holeName = "HOLE"

  private val envCompMap = envComps.map(x => x.name -> x).toMap
  def assembleRecProgram(term: Term): ComponentImpl = {
    ComponentImpl.recursiveImpl(signature, envCompMap, argListCompare, term)
  }

  private val compMapWithHole = {
    val holeImpl = ComponentImpl(holeName, IS(), signature.returnType, impl = {
      case _ => throw ExecuteHoleException
    })
    envCompMap.updated(holeName, holeImpl)
  }


  def searchMin(cost: Int, currentGoal: IndexValueMap,
                recTermsOfReturnType: IS[Seq[(Term, ExtendedValueVec)]],
                fillTermToHole: Term => Term,
                isFirstBranch: Boolean): Option[(Int,Term)] = {
    if(cost <= 0) return None

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
      import DSL._
      def assembleTerm(tElse: Term): Term = {
        fillTermToHole(`if`(tCond)(tThen)(tElse))
      }

      val elseGoal = currentGoal -- trueKeys

      val partialImpl = // todo: need special check for this, maybe completely separate recursive terms from normal terms?
        ComponentImpl.recursiveImpl(signature, compMapWithHole, argListCompare, assembleTerm(holeName $()))

      val envWithPartialImpl = envCompMap.updated(signature.name, partialImpl)

      val newRecTermsOfCost = recTermsOfReturnType.map(_.map {
        case (term, vv) =>
          val newVV = vv.indices.map { i =>
            vv(i) match {
              case ValueUnknown =>
                val varMap = signature.argNames.zip(inputVector(i)).toMap
                try {
                  Term.executeTerm(varMap, envWithPartialImpl)(term)
                } catch {
                  case ExecuteHoleException => ValueUnknown
                }
              case tv: TermValue => tv
            }
          }
          term -> newVV
      })

      val costSoFar = cThen + cCond + ifCost
      val maxCostForElse = math.min(cost, minCostCandidate.map(_._1).getOrElse(Int.MaxValue) - 1) - costSoFar
      for (
        (cElse, tElse) <- searchMin(maxCostForElse, elseGoal, newRecTermsOfCost, assembleTerm, isFirstBranch = false)
      ) {

        val totalCost = cElse + costSoFar
        val t = `if`(tCond)(tThen)(tElse)

        minCostCandidate = Some(totalCost -> t)
      }
    }
    minCostCandidate match {
      case Some((c, t)) => buffered(FoundAtCost(c, t))
      case None => buffered(NotFoundUnderCost(cost))
    }
  }

}

object DynamicGoalSearch{
  case object ExecuteHoleException extends Exception
}
