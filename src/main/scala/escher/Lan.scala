package escher

/**
  * term := <br>
    | Var(name) <br>
    | Component(name, term, ..., term) <br>
    | if term then term else term <br>
  */
sealed trait Term

object Term {

  case class Var(name: String) extends Term

  case class Component(name: String, terms: List[Term]) extends Term

  case class If(condition: Term, thenBranch: Term, elseBranch: Term) extends Term

  def executeTerm(varMap: String => TermValue, compMap: String => ComponentImpl)(term: Term): TermValue = {
    term match {
      case Var(name) => varMap(name)
      case Component(name, terms) =>
        val args = terms.map(executeTerm(varMap, compMap))
        compMap(name).execute(args)
      case If(c, t, e) =>
        val cv = executeTerm(varMap,compMap)(c)
        cv match {
          case TermError => TermError
          case TermBool(true) => executeTerm(varMap, compMap)(t)
          case TermBool(false) => executeTerm(varMap, compMap)(e)
        }
    }
  }

  def executeTermDebug(varMap: String => TermValue, compMap: String => ComponentImpl, depth: Int = 0)(term: Term): TermValue = {
    println("  "*depth + ">> " + term)
    val v = term match {
      case Var(name) => varMap(name)
      case Component(name, terms) =>
        val args = terms.map(executeTermDebug(varMap, compMap, depth + 1))
        compMap(name).execute(args)
      case If(c, t, e) =>
        val cv = executeTermDebug(varMap,compMap, depth + 1)(c)
        cv match {
          case TermError => TermError
          case TermBool(true) => executeTermDebug(varMap, compMap, depth + 1)(t)
          case TermBool(false) => executeTermDebug(varMap, compMap, depth + 1)(e)
        }
    }
    println("  " * depth + s"--> $v")
    v
  }
}

/**
  * type := <br>
    | tVar <br>
    | typeConstructor[type, ... , type] <br>
    <br>
    Our type system is first-order, so function types are not permitted.
  */
sealed trait Type{
  override def toString: String = Type.show(this)
}


object Type {

  case class TVar(id: Int) extends Type

  case class TApply(value: TypeConstructor, params: List[Type]) extends Type

  def show(t: Type): String = t match {
    case TVar(id) => s"'$id"
    case TApply(tc, params) =>
      if(params.isEmpty) tc.toString
      else s"$tc${params.map(show).mkString("[",",","]")}"
  }

}
















