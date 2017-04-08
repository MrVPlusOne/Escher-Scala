package escher

import escher.Synthesis.{ValueMap, ValueVector}

import scala.collection.mutable


object MutableGoalGraph {
  sealed trait GoalGraphState

  object GoalGraphState{
    case object Unsolved extends GoalGraphState

    case class SolvedByResolver(resolver: Resolver) extends GoalGraphState

    case class SolvedByTerm(term: Term) extends GoalGraphState{
      override def toString: String = s"SolvedByTerm(${term.show})"
    }
  }

  class GoalGraph(val valueMap: ValueMap,
                  val parents: mutable.Set[Resolver] = mutable.Set(),
                  val children: mutable.Set[Resolver] = mutable.Set(),
                  var state: GoalGraphState = GoalGraphState.Unsolved){
    def show(exampleCount: Int): String = {
      s"[[${ValueMap.show(valueMap, exampleCount)}]]"
    }

    def isSolved(): Boolean = state != GoalGraphState.Unsolved

    override def toString: String = {
      s"GoalGraph(${valueMap.mapValues(_.show)})"
    }
  }


  case class Resolver(cond: GoalGraph, thenBranch: GoalGraph, elseBranch: GoalGraph,
                      parent: GoalGraph){
    def isSolved(): Boolean = cond.isSolved() && thenBranch.isSolved() && elseBranch.isSolved()

    def childrenForgetThis(): Unit ={
      cond.parents -= this
      thenBranch.parents -= this
      elseBranch.parents -= this
    }

    def childrenRememberThis(): Unit ={
      cond.parents += this
      thenBranch.parents += this
      elseBranch.parents += this
    }

    val subGoals: List[GoalGraph] = {
      List(cond, thenBranch, elseBranch)
    }

    override def toString: String = {
      s"IF $cond THEN $thenBranch ELSE $elseBranch"
    }

  }

  class GoalManager(initGoal: ValueMap, library: ValueMap => Option[Term],
                    exampleCount: Int, printer: (Int, String) => Unit){
    val root = new GoalGraph(initGoal)

    val solvedGoals: mutable.Map[ValueMap,GoalGraph] = mutable.Map()
    val unsolvedGoals: mutable.Map[ValueMap,GoalGraph] = mutable.Map(initGoal -> root)

    def getOrCreateGoal(valueMap: ValueMap): GoalGraph = {
      solvedGoals.getOrElse(valueMap,
        unsolvedGoals.getOrElse(valueMap,
          library(valueMap) match {
            case Some(term) =>
              val g = new GoalGraph(valueMap, state = GoalGraphState.SolvedByTerm(term))
              solvedGoals(valueMap) = g
              g
            case None =>
              val g = new GoalGraph(valueMap)
              unsolvedGoals(valueMap) = g
              g
          }
        )
      )
    }

    def insertNewTerm(valueVector: ValueVector, term: Term): Unit = {

      /** @param goalGraph assume goalGraph is not solved yet */
      def resolve(goalGraph: GoalGraph): Unit = {
        require(!goalGraph.isSolved())

        if(ValueMap.matchVector(goalGraph.valueMap, valueVector)){
          changeGoalToSolvedState(goalGraph, GoalGraphState.SolvedByTerm(term))
        }else {
          // try to add resolvers (split this goal)
          ValueMap.splitValueMap(goalGraph.valueMap, valueVector) match {
            case  Some((cond, thenBranch, elseBranch)) =>
              val thenGoal = new GoalGraph(thenBranch, state = GoalGraphState.SolvedByTerm(term))
              solvedGoals(thenBranch) = thenGoal
              val r = Resolver(getOrCreateGoal(cond), thenGoal, getOrCreateGoal(elseBranch), goalGraph)
              if(r.isSolved()){
                changeGoalToSolvedState(goalGraph, GoalGraphState.SolvedByResolver(r))
              }else {
                goalGraph.children.foreach { resolver =>
                  resolver.subGoals.foreach(g => if (!g.isSolved()) resolve(g))
                }

                if(goalGraph.isSolved())
                  return
                r.childrenRememberThis()
                goalGraph.children += r
              }
            case None =>
              goalGraph.children.foreach { resolver =>
                resolver.subGoals.foreach(g => if (!g.isSolved()) resolve(g))
              }
          }
        }
      }

      resolve(root)
    }

    def changeGoalToSolvedState(goal: GoalGraph, newState: GoalGraphState): Unit = {
      if(goal.isSolved()) return

      solvedGoals(goal.valueMap) = goal
      unsolvedGoals -= goal.valueMap

      val unsolvedParents = goal.parents.filterNot(_.isSolved())
      goal.state = newState
      unsolvedParents.foreach(resolver =>
        if(resolver.isSolved()){
          changeGoalToSolvedState(resolver.parent, GoalGraphState.SolvedByResolver(resolver))
        }
      )

      //todo: instead of remove all resolvers, we could choose to replace the current resolver with a simpler resolver
      goal.children.foreach(_.childrenForgetThis())
      goal.children.clear()
    }


    def printResolver(resolver: Resolver, indent: Int): Unit = {
      printer(indent, "Resolver\n")
      resolver.subGoals.foreach(g => printGoalGraph(g, indent+1))
    }

    def printGoalGraph(g: GoalGraph, indent: Int): Unit = {
      printer(indent, ValueMap.show(g.valueMap, exampleCount) + s": ${g.state} \n")
      g.children.foreach(r => printResolver(r, indent+1))
    }

    def printState(): Unit = {
      printer(0, "Goal Manager State\n")
      printer(1, s"Solved Goals: ${solvedGoals.values.map(_.show(exampleCount))}\n")
      printer(1, s"Unsolved Goals: ${unsolvedGoals.values.map(_.show(exampleCount))}\n")

      printGoalGraph(root, indent= 1)
    }

  }
}
