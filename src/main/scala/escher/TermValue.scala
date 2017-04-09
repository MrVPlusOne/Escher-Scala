package escher

import escher.TermValue.matchTApply
import escher.Type.{TApply, TFixedVar, TVar}


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

  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] //todo: consider removing this method

  def show: String

  def smallerThan(tv: TermValue): Boolean = {
    if(sizeCompare.isDefinedAt(tv))
      sizeCompare(tv)
    else false
  }

  def greaterThan(tv: TermValue): Boolean = tv smallerThan this

  /** Should return true only if the other value has the same type and the "size" of this value is smaller
    * than the other, by requiring one argument's size must decrease during recursive calls, we can ensure
    * the synthesized functions' termination */
  protected def sizeCompare: PartialFunction[TermValue, Boolean]

}

case object ValueError extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = Some(TypeSubst.empty)

  def show = "Err"

  val sizeCompare: PartialFunction[TermValue, Boolean] = { case _ => false}
}

case class ValueBool(value: Boolean) extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = matchTApply(ty,TBool.of())

  def show: String = if(value) "T" else "F"

  val sizeCompare: PartialFunction[TermValue, Boolean] = { case ValueBool(v1) => !value && v1 }
}

case class ValueInt(value: Int) extends TermValue{
  def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = matchTApply(ty, TInt.of())

  def show: String = value.toString

  val sizeCompare: PartialFunction[TermValue, Boolean] = {
    case ValueInt(v1) => math.abs(value) < math.abs(v1)
  }
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

  val sizeCompare: PartialFunction[TermValue, Boolean] = {
    case ValueList(v1) => elems.length < v1.length
  }
}

sealed trait BinaryTree[+T]{
  def size: Int

}

case class BinaryNode[T](tag: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]{
  def size: Int = 1 + left.size + right.size
}

case object BinaryLeaf extends BinaryTree[Nothing]{
  def size: Int = 0
}


/** Binary tree */
case class ValueTree(value: BinaryTree[TermValue]) extends TermValue{
  override def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = ???

  override def show: String = {
    def aux(t: BinaryTree[TermValue]): String = t match {
      case BinaryNode(v, left, right) =>
        s"(${v.show}: ${aux(left)}, ${aux(right)})"
      case BinaryLeaf => "L"
    }
    aux(value)
  }

  val sizeCompare: PartialFunction[TermValue, Boolean] = {
    case ValueTree(v1) => value.size < v1.size
  }
}

case class ValuePair(value: (TermValue, TermValue)) extends TermValue{
  override def matchTypeAux(ty: Type, freeId: () => Int): Option[TypeSubst] = ???

  override def show: String = value match {
    case (l,r) => s"(${l.show}, ${r.show})"
  }

  val sizeCompare: PartialFunction[TermValue, Boolean] = {
    case ValuePair(v1) =>
      (value._1 smallerThan v1._1) || (value._1 == v1._1 && value._2.smallerThan(v1._2))
  }
}

object TermValue{
  def matchTApply(ty: Type, target: TApply): Option[TypeSubst] = ty match {
    case TVar(id) => Some(TypeSubst(Map(id -> target)))
    case TFixedVar(id) => None
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
  val arity = 0
}

case object TBool extends BasicType{
  val name: String = "Bool"
}

case object TInt extends BasicType {
  val name: String = "Int"
}

case object TList extends TypeConstructor{
  val arity: Int = 1

  val name: String = "List"
}

case object TTree extends TypeConstructor {
  val arity: Int = 1

  val name: String = "Tree"
}

case object TPair extends TypeConstructor {
  val name: String = "Pair"

  val arity: Int = 2
}

case object TMap extends TypeConstructor {
  val name: String = "Map"

  val arity: Int = 2
}