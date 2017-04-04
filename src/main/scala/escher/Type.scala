package escher

import escher.Type.{TApply, TVar}


/**
  * type := <br>
  *| tVar <br>
  *| typeConstructor[type, ... , type] <br>
  *<br>
  *Our type system is first-order, so function types are not permitted.
  */
sealed trait Type{
  override def toString: String = Type.show(this)
  def varSet: Set[Int] = Type.varSet(this)

  def nextFreeId: Int = this match {
    case TVar(id) => id
    case TApply(_, params) => (0 :: params.map(_.nextFreeId)).max
  }
}

object Type {

  case class TVar(id: Int) extends Type

  case class TApply(value: TypeConstructor, params: List[Type]) extends Type

  def show(t: Type): String = t match {
    case TVar(id) => s"'$id"
    case TApply(tc, params) =>
      if(params.isEmpty) tc.name
      else s"${tc.name}${params.map(show).mkString("[",",","]")}"
  }

  def varSet(t: Type): Set[Int] = t match {
    case TVar(id) => Set(id)
    case TApply(_, params) => params.foldRight(Set[Int]()){
      case (t1, s) => s ++ varSet(t1)
    }
  }

}

case class TypeSubst(map: Map[Int, Type]){

  def apply(ty: Type): Type = ty match {
    case v@TVar(id) => map.getOrElse(id, v)
    case TApply(value, params) => TApply(value, params.map(apply))
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
