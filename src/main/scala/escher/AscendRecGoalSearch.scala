package escher

import BatchGoalSearch._
import escher.DynamicGoalSearch.ExecuteHoleException
import escher.Synthesis._

/**
  * Search programs in Ascending Recursive Form
  */
class AscendRecGoalSearch(maxCompCost: Int,
                          signature: ComponentSignature,
                          envComps: Set[ComponentImpl],
                          argListCompare: (ArgList, ArgList) => Boolean,
                          inputVector: IS[ArgList],
                          termsWithKnownVV: IS[List[(ValueVector, Term)]],
                          nonRecBoolTerms: IS[List[(ValueVector, Term)]]) {

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


  //noinspection ReplaceToWithUntil
  def searchMin(cost: Int,
                currentGoal: IndexValueMap,
                recTermsOfReturnType: IS[Seq[(Term, ExtendedValueVec)]],
                fillTermToHole: Term => Term,
                isFirstBranch: Boolean,
                prefixTrigger: Option[List[String]]): Option[(Int,Term)] = {
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
      termsWithKnownVV(c-1).foreach{ case (vv, term) =>
        if(IndexValueMap.matchVector(currentGoal, vv))
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
    for {
      cCond <- 1 to math.min(maxCompCost, cost - ifCost - 2) // minus 2 because cCond + cElse >= 2
      (condVec, tCond) <- nonRecBoolTerms(cCond - 1)
      (thenGoal, elseGoal) <- splitGoal(condVec, currentGoal)
      cThen <- 1 to math.min(maxCompCost, cost - ifCost - cCond - 1)
    } {
      val (trig, prefixTrigger1) = checkTrigger(tCond, prefixTrigger)
      if (trig)
        println("trigger cond branch!")

      var thenCandidates = termsWithKnownVV(cThen - 1).map(_._2)
      for ((tThen, thenEVec) <- recTermsOfReturnType(cThen - 1)) {
        import DSL._
        val partialImpl = ComponentImpl.recursiveImpl(signature, compMapWithHole, argListCompare,
          fillTermToHole(`if`(tCond)(tThen)(holeName $())))
        val envWithPartialImpl = envCompMap.updated(signature.name, partialImpl)

        def isCandidateForThen: Boolean = {
          thenGoal.foreach { case (i, v) =>
            thenEVec(i) match {
              case ValueUnknown =>
                val varMap = varMaps(i)
                try {
                  if (Term.executeTerm(varMap, envWithPartialImpl)(tThen) != v)
                    return false
                } catch {
                  case ExecuteHoleException => return false
                }
              case tv: TermValue => if (tv != v) return false
            }
          }
          true
        }

        if (isCandidateForThen)
          thenCandidates = tThen +: thenCandidates
      }

      for (tThen <- thenCandidates) {
        val (trig, prefixTrigger2) = checkTrigger(tThen, prefixTrigger1)
        if (trig) {
          println("trigger then branch!")

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
    }

    minCostCandidate match {
      case Some((c, t)) =>
        buffered(FoundAtCost(c, t))
      case None => buffered(NotFoundUnderCost(cost))
    }
  }

}
