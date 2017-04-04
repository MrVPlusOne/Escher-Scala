package escher

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
}

object Term {

  case class Var(name: String) extends Term

  case class Component(name: String, terms: IS[Term]) extends Term

  case class If(condition: Term, thenBranch: Term, elseBranch: Term) extends Term

  def executeTerm(varMap: String => TermValue, compMap: String => ComponentImpl)(term: Term): TermValue = {
    term match {
      case Var(name) => varMap(name)
      case Component(name, terms) =>
        val args = terms.map(executeTerm(varMap, compMap))
        compMap(name).execute(args, debug = false)
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
    println("  "*depth + ">> " + term)
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
    println("  " * depth + s"--> $v")
    v
  }
}





















