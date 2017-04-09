package escher

import escher.Synthesis.{ValueMap, ValueVector}


object ImmutableGoalGraph {

  sealed trait GraphNode{
    def show(exampleCount: Int): String = this match {
      case Unsolved(valueMap, _) =>
        s"Unsolved(${ValueMap.show(valueMap, exampleCount)})"
      case SolvedByTerm(term) =>
        s"SolvedByTerm(${term.show})"
      case SolvedByResolver(resolver) =>
        s"SolvedByResolver(${resolver.show})"
    }

    def isSolved: Boolean = this match {
      case _: Unsolved => false
      case _ => true
    }

    def toTerm: Term = this match {
      case SolvedByTerm(term) => term
      case SolvedByResolver(r) =>
        import DSL._
        `if`(r.cond.toTerm)(r.thenBranch.toTerm)(r.elseBranch.toTerm)
      case _ =>
        throw new Exception("Unsolved goals can't be convert to Term")
    }
  }

  sealed trait SolvedNode extends GraphNode

  case class Unsolved(valueMap: ValueMap, children: Set[Resolver]) extends GraphNode

  case class SolvedByTerm(term: Term) extends SolvedNode

  case class SolvedByResolver(resolver: Resolver) extends SolvedNode

  case class Resolver(condMap: ValueMap, cond: GraphNode, thenBranch: SolvedNode, elseBranch: GraphNode){
    val isSolved: Boolean = cond.isSolved && elseBranch.isSolved

    def condAndElse: List[GraphNode] = {
      List(cond, elseBranch)
    }

    def show: String = {
      s"IF $cond THEN $thenBranch ELSE $elseBranch"
    }
  }

  class GoalManager(initGoal: ValueMap, boolLibrary: ValueMap => Option[Term], valueLibrary: ValueMap => Option[Term],
                    exampleCount: Int, printer: (Int, String) => Unit){
    var root: GraphNode = Unsolved(initGoal, Set())

    def insertNewTerm(valueVector: ValueVector, term: Term): Unit = {

      def closeGoals(unsolved: Unsolved): GraphNode = {
        if (ValueMap.matchVector(unsolved.valueMap, valueVector)) {
          SolvedByTerm(term)
        } else {
          val newChildren = unsolved.children.map { r =>
            val List(c, e) = r.condAndElse.map {
              case u: Unsolved => closeGoals(u)
              case other => other
            }
            val r1 = Resolver(r.condMap, c, r.thenBranch, e)
            if (r1.isSolved)
              return SolvedByResolver(r1)
            else r1
          }
          unsolved.copy(children = newChildren)
        }
      }


      def splitGoal(unsolved: Unsolved): GraphNode = {
        ValueMap.splitValueMap(unsolved.valueMap, valueVector) match {
          case None =>
            unsolved
          case Some((cond, _, b2)) =>
            val split = if (!unsolved.children.forall(r => r.condMap != cond)) {
              //if this splitting is already presented
              Set()
            } else {
              val gCond = boolLibrary(cond) match {
                case Some(t) => SolvedByTerm(t)
                case None => Unsolved(cond, Set())
              }

              val gb1 = SolvedByTerm(term)
              val gb2 = valueLibrary(b2) match {
                case Some(t) => SolvedByTerm(t)
                case None => Unsolved(b2, Set())
              }

              val r = Resolver(cond, gCond, gb1, gb2)
              if (r.isSolved)
                return SolvedByResolver(r)
              else Set(r)
            }

            val newChildren = unsolved.children.map { r =>
              val r1 = splitResolver(r)
              if (r1.isSolved)
                return SolvedByResolver(r1)
              else r1
            }
            unsolved.copy(children = newChildren ++ split)
        }
      }

      def splitResolver(resolver: Resolver): Resolver = {
        resolver.elseBranch match {
          case u: Unsolved =>
            resolver.copy(elseBranch = splitGoal(u))
          case _ => resolver
        }
      }

      root match {
        case goal: Unsolved =>
          closeGoals(goal) match {
            case goal1: Unsolved =>
              root = splitGoal(goal1)
            case solved => root = solved
          }
        case _ =>
          throw new Exception("Root goal has already been solved.")
      }
    }


    def printResolver(resolver: Resolver, indent: Int): Unit = {
      import resolver._
      printer(indent, s"Resolver(condition = ${ValueMap.show(condMap, exampleCount)})\n")

      List(cond, thenBranch, elseBranch).foreach(g => printGraphNode(g, indent+1))
    }

    def printGraphNode(g: GraphNode, indent: Int): Unit = g match {
      case g: Unsolved =>
        printer(indent, s"Unsolved: ${ValueMap.show(g.valueMap, exampleCount)}\n")
        g.children.foreach(r => printResolver(r, indent+1))
      case SolvedByTerm(term) =>
        printer(indent, s"SolvedByTerm(${term.show})\n")
      case SolvedByResolver(r) =>
        printer(indent, s"SolvedByResolver: \n")
        printResolver(r, indent + 1)
    }

    def printState(): Unit = {
      printer(0, "Goal Manager State:\n")
      printGraphNode(root, indent= 1)
    }

    def synthesizedProgram: Term = {
      root.toTerm
    }

  }

}
