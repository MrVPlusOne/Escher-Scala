package escher

import escher.Synthesis._

import collection.{immutable, mutable}
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

  val holeName = "HOLE"

  val varMaps: IS[Map[String, TermValue]] = inputVector.indices.map{ i=> signature.argNames.zip(inputVector(i)).toMap }

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

  /** place break points at the results of this function to help debugging */
  def checkTrigger(term: Term, prefixTrigger: Option[List[String]]): (Boolean, Option[List[String]]) ={
    prefixTrigger match {
      case None => (false, None)
      case Some(ts) => ts match {
        case Nil => (false, None)
        case List(t) =>
          (t == term.show, None)
        case (h::t) =>
          if(term.show == h) (false, Some(t))
          else (false, None)
      }
    }
  }

//  var bufferHit, bufferMiss = 0

  def searchMin(cost: Int, currentGoal: IndexValueMap,
                recTermsOfReturnType: IS[Seq[(Term, ExtendedValueVec)]],
                fillTermToHole: Term => Term,
                isFirstBranch: Boolean,
//                buffer: mutable.Map[Set[Int], SearchResult] = mutable.Map(),
                prefixTrigger: Option[List[String]]
               ): Option[(Int,Term)] = {
    if(cost <= 0) return None

    val keySet = currentGoal.keySet
//    buffer.get(keySet).foreach {
//      case FoundAtCost(c, term) if c <= cost =>
//        bufferHit +=1
//        return Some(c -> term)
//      case NotFoundUnderCost(c) if c >= cost =>
//        bufferMiss += 1
//        return None
//      case _ =>
//        bufferMiss += 1
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
      cThen <- 1 to maxCost - ifCost - 2; // minus 2 because cCond + cElse >= 2
      (thenVec, tThen) <- termsOfCost(cThen)) {
      val (trig, prefixTrigger1) = checkTrigger(tThen, prefixTrigger)
      if(trig){
        println("trigger then branch!")
      }
      for ((vm, _, _) <- IndexValueMap.splitValueMap(currentGoal, thenVec);
           ((cCond, tCond), trueKeys) <- maxSatConditions(vm, boolOfVM) //todo: may need to increase searching space
      ) {
        val (trig, prefixTrigger2) = checkTrigger(tCond, prefixTrigger1)
        if(trig){
          println("trigger condition!")
          val costSoFar = cThen + cCond + ifCost
          val maxCostForElse = math.min(cost, minCostCandidate.map(_._1).getOrElse(Int.MaxValue) - 1) - costSoFar
          val target = "createNode(treeValue(@baseTree), tConcat(treeLeft(@baseTree), @inserted), tConcat(treeRight(@baseTree), @inserted))"
          val found = recTermsOfReturnType.take(maxCostForElse).exists(_.exists(x => x._1.show == target))
          println("found: " + found)
        }

        import DSL._
        def assembleTerm(tElse: Term): Term = {
          fillTermToHole(`if`(tCond)(tThen)(tElse))
        }

        val costSoFar = cThen + cCond + ifCost
        val maxCostForElse = math.min(cost, minCostCandidate.map(_._1).getOrElse(Int.MaxValue) - 1) - costSoFar

        val partialImpl =
          ComponentImpl.recursiveImpl(signature, compMapWithHole, argListCompare, assembleTerm(holeName $()))
        val envWithPartialImpl = envCompMap.updated(signature.name, partialImpl)
        val newRecTermsOfCost = recTermsOfReturnType.take(maxCostForElse).map(_.map {
          case (term, vv) =>
            val newVV = vv.indices.map { i =>
              vv(i) match {
                case ValueUnknown =>
                  val varMap = varMaps(i)
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

//        val bufferForNextSearch = buffer.clone()
        val elseGoal = currentGoal -- trueKeys
        for (
          (cElse, tElse) <- searchMin(maxCostForElse, elseGoal, newRecTermsOfCost, assembleTerm,
            isFirstBranch = false, prefixTrigger = prefixTrigger2)
        ) {

          val totalCost = cElse + costSoFar
          val t = `if`(tCond)(tThen)(tElse)

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

object DynamicGoalSearch{
  case object ExecuteHoleException extends Exception
}
