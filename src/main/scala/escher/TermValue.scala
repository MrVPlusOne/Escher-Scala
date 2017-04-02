package escher

import escher.Type.TApply


/** Concrete value in a term */
trait TermValue {

}

case object TermError extends TermValue

case class TermBool(value: Boolean) extends TermValue{
}

case class TermInt(value: Int) extends TermValue{
}

case class TermList(elems: List[TermValue]) extends TermValue{
}


trait TypeConstructor{
  def arity: Int
  def of(params: Type*) = TApply(this, params.toList)
}

trait BasicType extends TypeConstructor{
  def arity = 0
}

case object TBool extends BasicType

case object TInt extends BasicType

case object TList extends TypeConstructor{
  def arity: Int = 1
}