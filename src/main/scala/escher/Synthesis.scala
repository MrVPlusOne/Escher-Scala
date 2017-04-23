package escher

import escher.Term.Component

import scala.collection.mutable


/**
  * The program synthesizing algorithm
  */
object Synthesis {
  type ValueVector = IndexedSeq[TermValue]
  type ArgList = IndexedSeq[TermValue]
  type IndexValueMap = Map[Int, TermValue]
  type ValueTermMap = mutable.Map[ValueVector, Term]

  def notAllErr(valueVector: ValueVector): Boolean = {
    ! valueVector.forall(_ == ValueError)
  }

  def showValueTermMap(valueTermMap: ValueTermMap): String = {
    val compList = valueTermMap.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
    compList.mkString("{", ", ", "}")
  }

  object IndexValueMap{
    def matchVector(valueMap: IndexValueMap, valueVector: ValueVector): Boolean = {
      valueMap.forall{
        case (k, v) => valueVector(k) == v
      }
    }

    def splitValueMap(valueMap: IndexValueMap, valueVector: ValueVector): Option[(IndexValueMap, IndexValueMap, IndexValueMap)] = {
      var thenMap: IndexValueMap = Map()
      var elseMap: IndexValueMap = Map()

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

    def show(valueMap: IndexValueMap, exampleCount: Int): String = {
      (0 until exampleCount).map(i => valueMap.get(i).map(_.show).getOrElse("?")).mkString("<", ", ", ">")
    }
  }

  object ArgList{
    /** @return true if and only if arg list 1 is alphabetically smaller than arg list 2 */
    def alphabeticSmaller(args1: ArgList, args2: ArgList): Boolean = {
      require(args1.length == args2.length)
      args1.indices.foreach{ i =>
        if(args1(i) greaterThan args2(i))
          return false
        else if(args1(i) smallerThan args2(i))
          return true
      }
      false
    }

    /** @return true if and only if every arg in arg list 1 is not bigger than corresponding args in list 2
      *         and at least one of them is strictly smaller */
    def anyArgSmaller(args1: ArgList, args2: ArgList): Boolean = {
      require(args1.length == args2.length)
      var smaller = false
      args1.indices.foreach{ i =>
        if(args1(i) greaterThan args2(i))
          return false
        else if(args1(i) smallerThan args2(i))
          smaller = true
      }
      smaller
    }

    def showArgList(argList: ArgList): String = {
      argList.map(_.show).mkString("(",", ",")")
    }
  }

  object ValueVector{
    def show(valueVector: ValueVector): String = {
      valueVector.map(_.show).mkString("<",", ",">")
    }
  }

  def showExamples(examples: Seq[(ArgList, TermValue)]): String = {
    examples.map{ case (a,r) => s"${ArgList.showArgList(a)} -> ${r.show}" }.mkString("; ")
  }


  def divideNumberAsSum(number: Int, pieces: Int, minNumber: Int): IS[IndexedSeq[Int]] = {
    if(number<minNumber) return IS()
    if(pieces == 1) return IS(IndexedSeq(number))

    (minNumber to number).toVector.flatMap(n => divideNumberAsSum(number - n, pieces - 1, minNumber).map(n +: _))
  }

  def cartesianProduct[A](listOfSets: IndexedSeq[Iterable[A]]): Iterator[IndexedSeq[A]] = {
    if(listOfSets.isEmpty) return Iterator(IndexedSeq())
    listOfSets.head.toIterator.flatMap(v => cartesianProduct(listOfSets.tail).map(v +: _))
  }

  case class SynthesizedComponent(name: String, argNames: IS[String],
                                  inputTypes: IS[Type], returnType: Type,
                                  body: Term, cost: Int, depth: Int){
    def show: String = {
      val paramList = argNames.zip(inputTypes).map{
        case (argName, ty) => s"@$argName: $ty"
      }
      s"$name(${paramList.mkString(", ")}): $returnType =\n  ${body.show}"
    }

    def print(): Unit = {
      val paramList = argNames.zip(inputTypes).map{
        case (argName, ty) => s"@$argName: $ty"
      }
      println(s"$name(${paramList.mkString(", ")}): $returnType =")
      Term.printTerm(body, 2)
    }
  }

  class BufferedOracle(val examples: IS[(ArgList, TermValue)] , oracle: PartialFunction[ArgList, TermValue],
                       initBuffer: IS[(ArgList, TermValue)]){
    val knownMap: Map[ArgList, TermValue] = examples.toMap
    private val _buffer = mutable.Map[ArgList, TermValue](initBuffer :_*)

    def buffer: Map[IS[TermValue], TermValue] = _buffer.toMap

    def evaluate(argList: ArgList): TermValue = {
      knownMap.getOrElse(argList,
        buffer.getOrElse(argList, {
          val result = oracle(argList)
          _buffer(argList) = result
          result
        })
      )
    }

  }


  def exampleLt(ex1: (ArgList, TermValue), ex2: (ArgList, TermValue)): Boolean = {
    ArgList.alphabeticSmaller(ex1._1, ex2._1)
  }

  trait RebootStrategy{
    /** @return (newExamples, newOracleBuffer)
      */
    def newExamplesAndOracleBuffer(examples: IS[(ArgList, TermValue)],
                                   failed: IS[(ArgList, TermValue)],
                                   passed: IS[(ArgList, TermValue)]):
    (IS[(ArgList, TermValue)], IS[(ArgList, TermValue)])
  }

  object RebootStrategy{
    val addSimplestFailedExample = new RebootStrategy {

      /** @return (newExamples, newOracleBuffer)
        */
      override def newExamplesAndOracleBuffer(examples: IS[(ArgList, TermValue)],
                                              failed: IS[(ArgList, TermValue)],
                                              passed: IS[(ArgList, TermValue)]):
      (IS[(ArgList, TermValue)], IS[(ArgList, TermValue)]) = {
        val failSorted = failed.sortWith(exampleLt)
        (examples :+ failSorted.head, failSorted.tail ++ passed)
      }
    }

    /** this works surprisingly bad */
    val addMostComplicatedFailedExample = new RebootStrategy {

      /** @return (newExamples, newOracleBuffer)
        */
      override def newExamplesAndOracleBuffer(examples: IS[(ArgList, TermValue)],
                                              failed: IS[(ArgList, TermValue)],
                                              passed: IS[(ArgList, TermValue)]):
      (IS[(ArgList, TermValue)], IS[(ArgList, TermValue)]) = {
        val failSorted = failed.sortWith(exampleLt)
        (examples :+ failSorted.last,  passed ++ failSorted.init)
      }
    }
  }




}

