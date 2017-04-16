package escher

import collection.mutable
import ValueVectorTree._
import escher.Synthesis.{ValueMap, ValueVector}

/**
  * A tree with fixed depth, each path from root to leaf represents a ValueVector,
  * can be used to efficiently find ValueVectors that match a goal (partial vector)
  */
object ValueVectorTree {

  sealed trait TreeNode[A]{
  }

  case class LeafNode[A](term: A) extends TreeNode[A]


  class InternalNode[A](val children: mutable.Map[TermValue, TreeNode[A]]) extends TreeNode[A] {
    def addTerm(term: A, valueVector: List[TermValue]): Boolean = valueVector match {
      case Nil => throw new Exception("Empty valueVector!")
      case List(v) =>
        if(children.contains(v)) false
        else {
          children(v) = LeafNode(term)
          true
        }
      case v :: tail =>
        val n1 = children.get(v) match {
          case Some(n1: InternalNode[A]) => n1
          case None => new InternalNode[A](mutable.Map())
          case _ => throw new Exception()
        }
        children(v) = n1
        n1.addTerm(term, tail)
        true
    }

    def searchTerms(valueVector: List[Either[Unit ,TermValue]]): Iterator[A] = valueVector match {
      case Nil => throw new Exception("Empty valueVector!")
      case List(v) => v match {
        case Left(_) =>
          children.values.toIterator.collect {
            case LeafNode(t) => t
          }
        case Right(tv) =>
          children.get(tv) match {
            case Some(LeafNode(t)) => Iterator(t)
            case _ => Iterator()
          }
      }
      case v :: tail => v match {
        case Left(_) =>
          children.values.toIterator.flatMap {
            case n1: InternalNode[A] =>
              n1.searchTerms(tail)
            case _ => throw new Exception()
          }
        case Right(tv) =>
          children.get(tv) match {
            case Some(n1: InternalNode[A]) => n1.searchTerms(tail)
            case _ => Iterator()
          }
      }
    }

    def searchATerm(valueVector: List[Either[Unit ,TermValue]]): Option[A] = valueVector match {
      case Nil => throw new Exception("Empty valueVector!")
      case List(v) => v match {
        case Left(_) =>
          children.values.foreach {
            case LeafNode(t) => return Some(t)
            case _ => sys.error("ill-formed tree.")
          }
          None
        case Right(tv) =>
          children.get(tv) match {
            case Some(LeafNode(t)) => Some(t)
            case _ => None
          }
      }
      case v :: tail => v match {
        case Left(_) =>
          children.values.foreach {
            case n1: InternalNode[A] =>
              n1.searchATerm(tail).foreach(t => return Some(t))
            case _ => throw new Exception()
          }
          None
        case Right(tv) =>
          children.get(tv) match {
            case Some(n1: InternalNode[A]) => n1.searchATerm(tail)
            case _ => None
          }
      }
    }

  }

  def print[T](tree: TreeNode[T], show: T => String, indent: Int): Unit = tree match {
    case LeafNode(term) =>
      println("  " * indent + s"* ${show(term)}")
    case in : InternalNode[T] =>
      in.children.foreach{
        case (tv, tree1) =>
          println("  " * indent + s"- ${tv.show}")
          print(tree1, show, indent + 1)
      }
  }

}

/**
  * A tree with fixed depth, each path from root to leaf represents a ValueVector,
  * can be used to efficiently find ValueVectors that match a goal (partial vector)
  * @param depth the length of each ValueVector
  */
class ValueVectorTree[A](depth: Int){
  private var _size = 0
  def size: Int = _size

  val root = new InternalNode[A](mutable.Map())

  private var _elements = List[(ValueVector, A)]()
  def elements: List[(ValueVector, A)] = _elements

  def addTerm(term: A, valueVector: ValueVector): Boolean = {
    val added = root.addTerm(term, valueVector.toList)
    if(added) {
      _size += 1
      _elements = (valueVector -> term) :: _elements
    }
    added
  }

  def update(valueVector: ValueVector, term: A): Boolean = addTerm(term, valueVector)

  private def valueMapToVector(valueMap: ValueMap): List[Either[Unit, TermValue]] ={
    (0 until depth).toList.map{ i =>
      valueMap.get(i) match{
        case Some(tv) => Right(tv)
        case None => Left(())
      }
    }
  }

  def searchTerms(valueMap: ValueMap): Iterator[A] = {
    val vv = valueMapToVector(valueMap)
    root.searchTerms(vv)
  }

  def searchATerm(valueMap: ValueMap): Option[A] = {
    val vv = valueMapToVector(valueMap)
    root.searchATerm(vv)
  }

  def get(valueVector: ValueVector): Option[A] = {
    root.searchATerm(valueVector.toList.map(tv => Right(tv)))
  }

  def printRoot(show: A => String): Unit ={
    println("---Root Status---")
    print(root, show, 0)
  }
}