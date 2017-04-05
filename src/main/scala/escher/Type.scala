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
    case TVar(id) => id + 1
    case TFixedVar(_) => 0
    case TApply(_, params) => (0 :: params.map(_.nextFreeId)).max
  }

  def shiftId(amount: Int): Type = renameIndices(_ + amount)

  def renameIndices(f: Int => Int): Type = this match {
    case TVar(id) => TVar(f(id))
    case TApply(contr, params) => TApply(contr, params.map(_.renameIndices(f)))
    case fv: TFixedVar => fv
  }

  def instanceOf(that: Type): Boolean = Type.instanceOf(this, that)
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

  /** Two types that are alpha-equivalence are guaranteed to have same alpha-normalization form,
    * good for being used as Map keys */
  def alphaNormalize(t: Type): Type = {
    import collection.mutable

    var nextIndex = 0
    val map = mutable.Map[Int,Int]()

    def rec(t: Type): Unit = t match {
      case TVar(id) =>
        map.getOrElse(id, {
          map(id) = nextIndex
          nextIndex += 1
        })
      case TFixedVar(id) => // nothing
      case TApply(contr, params) =>
        params.foreach(rec)
    }

    rec(t)
    t.renameIndices(map.apply)
  }

  /** return the most general unifier of two types (if exist),
    * prefer to rename vars in the first argument over the second */
  def unify(ty1: Type, ty2: Type): Option[TypeSubst] = ty1 match {
    case TVar(id) => Some(TypeSubst(Map(id -> ty2)))
    case _ => ty2 match {
      case TVar(id) => Some(TypeSubst(Map(id -> ty1)))
      case _ => ty1 match {
        case TFixedVar(_) => if(ty2 == ty1) Some(TypeSubst.empty) else None
        case TApply(contr, params) => ty2 match{
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

  def instanceOf(t: Type, parent: Type): Boolean = {
    val pFreeId = parent.nextFreeId
    val t1 = t.shiftId(pFreeId)

    Type.unify(parent, t1) match {
      case Some(unifier) =>
        unifier(t1) == t1
      case None => false
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
