package escher

import escher.Term.Component

import scala.collection.mutable


/**
  * The program synthesizing algorithm
  */
object Synthesis {
  type ValueVector = IndexedSeq[TermValue]
  type Input = IndexedSeq[TermValue]
  type ValueMap = Map[Int, TermValue]
  type ValueTermMap = mutable.Map[ValueVector, Term]

  def notAllErr(valueVector: ValueVector): Boolean = {
    ! valueVector.forall(_ == ValueError)
  }

  def showValueTermMap(valueTermMap: ValueTermMap): String = {
    val compList = valueTermMap.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
    compList.mkString("{", ", ", "}")
  }

  object ValueMap{
    def show(valueMap: ValueMap, exampleCount: Int): String = {
      (0 until exampleCount).map(i => valueMap.get(i).map(_.show).getOrElse("?")).mkString("<", ", ", ">")
    }
  }

  object ValueVector{
    def show(valueVector: ValueVector): String = {
      valueVector.map(_.show).mkString("<",", ",">")
    }
  }


  class GoalGraph(valueMap: ValueMap){
    def show(exampleCount: Int): String = {
      s"[[${ValueMap.show(valueMap, exampleCount)}]]"
    }
  }



  def divideNumberAsSum(number: Int, pieces: Int, minNumber: Int): Iterator[IndexedSeq[Int]] = {
    if(number<minNumber) return Iterator()
    if(pieces == 1) return Iterator(IndexedSeq(number))

    (minNumber to number).toIterator.flatMap(n => divideNumberAsSum(number - n, pieces - 1, minNumber).map(n +: _))
  }

  def cartesianProduct[A](listOfSets: IndexedSeq[Iterable[A]]): Iterator[IndexedSeq[A]] = {
    if(listOfSets.isEmpty) return Iterator(IndexedSeq())
    listOfSets.head.toIterator.flatMap(v => cartesianProduct(listOfSets.tail).map(v +: _))
  }

}

