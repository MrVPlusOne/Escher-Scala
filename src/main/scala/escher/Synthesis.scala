package escher

import escher.Term.Component

import scala.collection.mutable


/**
  * The program synthesizing algorithm
  */
object Synthesis {
  type ValueVector = IndexedSeq[TermValue]
  type ArgList = IndexedSeq[TermValue]
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
    def matchVector(valueMap: ValueMap, valueVector: ValueVector): Boolean = {
      valueMap.forall{
        case (k, v) => valueVector(k) == v
      }
    }

    def splitValueMap(valueMap: ValueMap, valueVector: ValueVector): Option[(ValueMap, ValueMap, ValueMap)] = {
      var thenMap: ValueMap = Map()
      var elseMap: ValueMap = Map()

      val condMap = valueMap.map{
        case (i, v) =>
        val `match` = valueVector(i) == v
        if(`match`)
          thenMap = thenMap.updated(i, v)
        else
          elseMap = elseMap.updated(i, v)
        i -> ValueBool(`match`)
      }

      if(thenMap.nonEmpty && elseMap.nonEmpty)
        Some((condMap, thenMap, elseMap))
      else
        None
    }

    def show(valueMap: ValueMap, exampleCount: Int): String = {
      (0 until exampleCount).map(i => valueMap.get(i).map(_.show).getOrElse("?")).mkString("<", ", ", ">")
    }
  }

  object ArgList{
    def alphabeticSmallerThan(args1: ArgList, args2: ArgList): Boolean = {
      require(args1.length == args2.length)
      args1.indices.foreach{ i =>
        if(args1(i) greaterThan args2(i))
          return false
        else if(args1(i) smallerThan args2(i))
          return true
      }
      false
    }
  }

  object ValueVector{
    def show(valueVector: ValueVector): String = {
      valueVector.map(_.show).mkString("<",", ",">")
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

  case class SynthesizedComponent(name: String, argNames: IS[String],
                                  inputTypes: IS[Type], returnType: Type,
                                  body: Term){
    def show: String = {
      val paramList = argNames.zip(inputTypes).map{
        case (argName, ty) => s"@$argName: $ty"
      }
      s"$name(${paramList.mkString(", ")}): $returnType =\n  ${body.show}"
    }
  }

}

