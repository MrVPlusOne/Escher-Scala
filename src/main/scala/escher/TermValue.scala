package escher

import escher.TermValue.matchTApply
import escher.Type.{TApply, TVar}


/** The computational result to which a term can evaluate */
trait TermValue {

  def matchType(ty: Type, freeId: Int): Option[(TypeSubst, Int)] = {
    var id = freeId
    def counter(): Int = {
      val ret = id
      id = ret + 1
      ret
    }
    matchTypeAux(ty, counter).map{ s => (s, id)}
  }

  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst]

  def show: String
}

case object ValueError extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = Some(TypeSubst.empty)

  def show = "Err"
}

case class ValueBool(value: Boolean) extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = matchTApply(ty,TBool.of())

  def show: String = value.toString
}

case class ValueInt(value: Int) extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = matchTApply(ty, TInt.of())

  def show: String = value.toString
}

case class ValueList(elems: List[TermValue]) extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = ty match {
    case TVar(id) =>
      val i1 = freeId()
      matchTypeAux(TList of TVar(i1), freeId).map{ s =>
        TypeSubst(Map(id -> (TList of TVar(i1)))).compose(s)
      }
    case TApply(TList, List(elemType)) =>
      val s = elems.foldLeft(TypeSubst.empty){
        case (subst, term) =>
          term.matchTypeAux(subst(elemType), freeId) match{
            case None => return None
            case Some(s1) => subst compose s1
          }
      }
      Some(s)
    case _ => None
  }

  def show: String = elems.map(_.show).mkString("[", ", ", "]")
}

object TermValue{
  def matchTApply(ty: Type, target: TApply): Option[TypeSubst] = ty match {
    case TVar(id) => Some(TypeSubst(Map(id -> target)))
    case TApply(_, _) =>
      if(ty == target) Some(TypeSubst.empty)
      else None
  }
}



trait TypeConstructor{
  def name: String
  def arity: Int
  def of(params: Type*): TApply = {
    require(params.length == arity)
    TApply(this, params.toList)
  }
}

trait BasicType extends TypeConstructor{
  def arity = 0
}

case object TBool extends BasicType{
  override def name: String = "bool"
}

case object TInt extends BasicType {
  override def name: String = "int"
}

case object TList extends TypeConstructor{
  def arity: Int = 1

  override def name: String = "list"
}

case object TMap extends TypeConstructor{
  override def name: String = "map"

  override def arity: Int = 2
}