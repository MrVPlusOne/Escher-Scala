package escher

import escher.SynNoOracle.ExtendedCompImpl
import escher.Term.{Component, If, Var}

/**
  * term := <br>
    *| Var(name) <br>
    *| Component(name, term, ..., term) <br>
    *| if term then term else term <br>
  */
sealed trait Term{
  def show: String = this match {
    case Var(name) => s"@$name"
    case Component(name, terms) => s"$name(${terms.map(_.show).mkString(", ")})"
    case If(condition, thenBranch, elseBranch) =>
      s"if ${condition.show} then ${thenBranch.show} else ${elseBranch.show}"
  }

  def kind: Int

  def <(that: Term): Boolean = Term.lt(this, that)
  def >(that: Term): Boolean = Term.lt(that, this)
}

object Term {

  case class Var(name: String) extends Term{
    def kind = 0
  }

  case class Component(name: String, terms: IS[Term]) extends Term{
    def kind = 1
  }

  case class If(condition: Term, thenBranch: Term, elseBranch: Term) extends Term{
    def kind = 2
  }

  def termsLt(terms1: IS[Term], terms2: IS[Term]): Boolean = {
    terms1.indices.foreach(i => {
      if(lt(terms2(i), terms1(i)))
        return false
      else if(lt(terms1(i), terms2(i)))
        return true
    })
    false
  }

  def lt(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (Var(n1), Var(n2)) => n1 < n2
    case (Component(n1, ts1), Component(n2, ts2)) =>
      n1 < n2 || ((n1 == n2) && termsLt(ts1, ts2))
    case (If(a1,b1,c1), If(a2,b2,c2)) =>
      termsLt(IS(a1,b1,c1), IS(a2,b2,c2))
    case (a,b) => a.kind < b.kind
  }

  def executeTerm(varMap: String => TermValue, compMap: Map[String, ComponentImpl])(term: Term): TermValue = {
    term match {
      case Var(name) => varMap(name)
      case Component(name, terms) =>
        val args = terms.map(executeTerm(varMap, compMap))
        compMap(name).executeEfficient(args)
      case If(c, t, e) =>
        val cv = executeTerm(varMap,compMap)(c)
        cv match {
          case ValueError => ValueError
          case ValueBool(true) => executeTerm(varMap, compMap)(t)
          case ValueBool(false) => executeTerm(varMap, compMap)(e)
        }
    }
  }

  def executeTermDebug(varMap: Map[String, TermValue], compMap: Map[String, ComponentImpl], depth: Int = 0)(term: Term): TermValue = {
    println("  "*depth + ">> " + term.show)
    val v = term match {
      case Var(name) => varMap.getOrElse(name, throw ExecutionError(s"variable '$name' not in scope!"))
      case Component(name, terms) =>
        val args = terms.map(executeTermDebug(varMap, compMap, depth + 1))
        compMap.getOrElse(name, throw ExecutionError(s"component '$name' not in scope!")).execute(args, debug = true)
      case If(c, t, e) =>
        val cv = executeTermDebug(varMap,compMap, depth + 1)(c)
        cv match {
          case ValueError => ValueError
          case ValueBool(true) => executeTermDebug(varMap, compMap, depth + 1)(t)
          case ValueBool(false) => executeTermDebug(varMap, compMap, depth + 1)(e)
        }
    }
    println("  " * depth + s"--> ${v.show}")
    v
  }

  def executeTermInExtendedEnv(varMap: Map[String, ExtendedValue], compMap: Map[String, ExtendedCompImpl])(term: Term): ExtendedValue = {
    term match {
      case Var(name) => varMap(name)
      case Component(name, terms) =>
        val args = terms.map { t =>
          executeTermInExtendedEnv(varMap, compMap)(t) match {
            case ValueUnknown => return ValueUnknown
            case tv: TermValue => tv
          }
        }

        compMap(name).execute(args)
      case If(c, t, e) =>
        val cv = executeTermInExtendedEnv(varMap, compMap)(c)
        cv match {
          case ValueUnknown => ValueUnknown
          case ValueError => ValueError
          case ValueBool(true) => executeTermInExtendedEnv(varMap, compMap)(t)
          case ValueBool(false) => executeTermInExtendedEnv(varMap, compMap)(e)
        }
    }
  }

  def printTerm(term: Term, depth: Int = 0): Unit = {
    def printTermAux(term: Term, depth: Int): Unit = term match {
      case If(condition, thenBranch, elseBranch) =>
        val prefix = " " * depth
        println(s"if ${condition.show}")
        print(s"${prefix}then ")
        printTermAux(thenBranch, depth+5)
        print(s"${prefix}else ")
        printTermAux(elseBranch, depth+5)
      case other =>
        println(other.show)
    }

    print(" "*depth)
    printTermAux(term, depth)
  }


}





















