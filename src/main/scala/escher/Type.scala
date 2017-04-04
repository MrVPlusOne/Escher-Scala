package escher

import escher.Type.{TApply, TFixedVar, TVar}


/**
  * type := <br>
  *| tVar <br>
  *| typeConstructor[type, ... , type] <br>
  *<br>
  *Our type system is first-order, so function types are not permitted.
  */
sealed trait Type{
  override def toString: String = Type.show(this)
  def varSet: Set[Int] = Type.freeVarSet(this)

  def nextFreeId: Int = this match {
    case TVar(id) => id
    case TFixedVar(_) => 0
    case TApply(_, params) => (0 :: params.map(_.nextFreeId)).max
  }

  def shiftId(amount: Int): Type = this match {
    case TVar(id) => TVar(id+amount)
    case TFixedVar(id) => TFixedVar(id)
    case TApply(contr, params) => TApply(contr, params.map(_.shiftId(amount)))
  }
}

object Type {

  /** a TVar in Component signatures can vary to match any type */
  case class TVar(id: Int) extends Type

  /** a TFixedVar shouldn't appear in Component signatures, it's only used in the synthesis program to
    * denote fixed type parameters of the Component currently being synthesized */
  case class TFixedVar(id: Int) extends Type

  case class TApply(contr: TypeConstructor, params: List[Type]) extends Type

  def show(t: Type): String = t match {
    case TVar(id) => s"?$id"
    case TFixedVar(id) => s"'$id"
    case TApply(tc, params) =>
      if(params.isEmpty) tc.name
      else s"${tc.name}${params.map(show).mkString("[",",","]")}"
  }

  def freeVarSet(t: Type): Set[Int] = t match {
    case TVar(id) => Set(id)
    case TFixedVar(_) => Set()
    case TApply(_, params) => params.foldRight(Set[Int]()){
      case (t1, s) => s ++ freeVarSet(t1)
    }
  }

  def unify(ty: Type, target: Type): Option[TypeSubst] = ty match {
    case TVar(id) => Some(TypeSubst(Map(id -> target)))
    case _ => target match {
      case TVar(id) => Some(TypeSubst(Map(id -> ty)))
      case _ => ty match {
        case TFixedVar(_) => if(target == ty) Some(TypeSubst.empty) else None
        case TApply(contr, params) => target match{
          case TApply(contr2, params2) if contr == contr2 =>
            val r = params.zip(params2).foldLeft(TypeSubst.empty){
              case (subst, (t1,t2)) =>
                unify(subst(t1), subst(t2)) match {
                  case None => return None
                  case Some(ts) => subst compose ts
                }
            }
            Some(r)
          case _ => None
        }
        case _ => throw new Exception("Not possible")
      }
    }
  }

}

case class TypeSubst(map: Map[Int, Type]){

  def apply(ty: Type): Type = ty match {
    case v@TVar(id) => map.getOrElse(id, v)
    case TApply(value, params) => TApply(value, params.map(apply))
    case TFixedVar(_) => ty
  }

  /** the standard substitution composition */
  def compose(that: TypeSubst): TypeSubst = {
    TypeSubst(that.map ++ map.mapValues(that.apply))
  }

  def deleteVar(i1: Int): TypeSubst = {
    TypeSubst(map - i1)
  }

  def contains(subst: TypeSubst): Boolean = {
    subst.map subMapOf this.map
  }
}

object TypeSubst{
  def empty: TypeSubst = TypeSubst(Map())
}
