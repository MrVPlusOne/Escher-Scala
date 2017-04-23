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

  def fixVars: Type = this match {
    case TVar(id) => TFixedVar(id)
    case TFixedVar(id) => TFixedVar(id)
    case TApply(contr, params) => TApply(contr, params.map(_.fixVars))
  }

  def containsVar(id: Int): Boolean = this match {
    case TVar(id1) => id == id1
    case TFixedVar(_) => false
    case TApply(_, params) => !params.forall(t => !t.containsVar(id))
  }

  def containsFixedVar(id: Int): Boolean = this match {
    case TFixedVar(id1) => id == id1
    case TVar(_) => false
    case TApply(_, params) => !params.forall(t => !t.containsFixedVar(id))
  }

  def canAppearIn(bigType: Type): Boolean = Type.canAppearIn(this, bigType)
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

  /** Two alpha-equivalent types are guaranteed to have same alpha-normalization form,
    * good for being used as Map keys */
  def alphaNormalForm(t: Type): Type = {
    val map = alphaNormalRenaming(t)
    t.renameIndices(map.apply)
  }

  /** Two alpha-equivalent types are guaranteed to have same alpha-normalization form,
    * good for being used as Map keys
    *
    * @return an indices renaming map
    */
  def alphaNormalRenaming(t: Type): Map[Int, Int] = {
    var nextIndex = 0
    var map = Map[Int,Int]()

    def rec(t: Type): Unit = t match {
      case TVar(id) =>
        map.getOrElse(id, {
          map = map.updated(id, nextIndex)
          nextIndex += 1
        })
      case TFixedVar(id) => // nothing
      case TApply(contr, params) =>
        params.foreach(rec)
    }

    rec(t)
    map
  }

  /** return the most general unifier of two types (if exist),
    * prefer to rename vars in the first argument over the second */
  def unify(ty1: Type, ty2: Type): Option[TypeSubst] = {
    if(ty1 == ty2) return Some(TypeSubst.empty)
    ty1 match {
      case TVar(id) => if(ty2 containsVar id) None else Some(TypeSubst(Map(id -> ty2)))
      case _ => ty2 match {
        case TVar(id) => if(ty1 containsVar id) None else Some(TypeSubst(Map(id -> ty1)))
        case TFixedVar(_) => None
        case _ => ty1 match {
          case TFixedVar(_) => None
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
  }

  def instanceOf(t: Type, parent: Type): Boolean = instanceOfWithMap(t, parent).nonEmpty

  def instanceOfWithMap(t: Type, parent: Type): Option[TypeSubst] = {
    val pFreeId = parent.nextFreeId
    val t1 = t.shiftId(pFreeId)

    Type.unify(parent, t1) match {
      case Some(unifier) =>
        if(unifier(t1) == t1) Some(unifier)
        else None
      case None => None
    }
  }

  def canAppearIn(smallType: Type, bigType: Type): Boolean = (smallType, bigType) match {
    case (TVar(_), _) => true
    case (TFixedVar(id), _) => bigType.containsFixedVar(id)
    case (a1@TApply(contr1, _), a2@ TApply(contr2, params2)) =>
      if(contr1 == contr2 && a2.instanceOf(a1))
        return true
      params2.foreach(p => if(canAppearIn(a1, p)) return true)
      false
    case _ => false
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
