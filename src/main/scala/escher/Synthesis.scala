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


  class SynthesisState(val goalGraph: GoalGraph, var levelMaps: IndexedSeq[TypeMap], val totalMap: TypeMap) {
    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ TypeMap.empty
      levelMaps.length
    }

    def registerTermAtLevel(level: Int, term: Term, ty: Type, valueVector: ValueVector): Boolean = {
      totalMap(ty).get(valueVector) match {
        case None =>
          totalMap.registerTerm(term, ty, valueVector)
          levelMaps(level).registerTerm(term, ty, valueVector)
          true
        case Some(_) =>
          false
      }
    }

    def print(exampleCount: Int): Unit = {
      println(s"Goal: ${goalGraph.show(exampleCount)}")
      println(s"TotalMap: ${totalMap.show}")
      val levelsShow = levelMaps.map(_.show).zipWithIndex.map{
        case (s, level) => s"  $level: $s"
      }.mkString("\n")
      println(s"LevelMaps:\n $levelsShow")
    }

  }

  class TypeMap private(private val map: mutable.Map[Type, Map[ValueVector, Term]]){
    def isTypeUnlocked(ty: Type): Boolean = {
      map.contains(ty)
    }

    def unlockType(ty: Type): Unit ={
      map(ty) = map.getOrElse(ty, Map())
    }

    def apply(ty: Type): Map[ValueVector, Term] = {
      map(ty)
    }

    def registerTerm(term: Term, ty: Type, valueMap: ValueVector): Unit = {
      map(ty) = map(ty).updated(valueMap, term)
    }

    def show: String = {
      map.mapValues{map =>
        val compList = map.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
        compList.mkString("{", ", ", "}")
      }.toString
    }
  }
  object TypeMap{
    def empty: TypeMap = new TypeMap(mutable.Map())
  }

  class GoalGraph(valueMap: ValueMap){
    def show(exampleCount: Int): String = {
      s"[[${ValueMap.show(valueMap, exampleCount)}]]"
    }
  }

  def synthesize(name: String, inputTypes: IndexedSeq[Type], inputNames: IndexedSeq[String], outputType: Type)
                (compMap: Map[String, ComponentImpl], compCostFunction: ComponentImpl => Int,
                 inputs: IndexedSeq[Input], outputs: IndexedSeq[TermValue]) = {
    require(inputTypes.length == inputNames.length)

    import DSL._

    val exampleCount = outputs.length
    val state = new SynthesisState(
      new GoalGraph(outputs.zipWithIndex.map(_.swap).toMap),
      IndexedSeq(),
      TypeMap.empty
    )

    val arity = inputTypes.length
    state.openNextLevel()
    (0 until arity).foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      state.registerTermAtLevel(0, v(inputNames(argId)), inputTypes(argId), valueMap)
    })

    def synthesizeTypeAtLevel(level: Int, targetType: Type): Unit = {
      //fixme: need to properly handle polymorphism
      val shiftIdAmount = targetType.nextFreeId
      for(
        (compName, oldImpl) <- compMap;
        compCost = compCostFunction(oldImpl) if compCost <= level;
        impl = oldImpl.shiftTypeId(shiftIdAmount);
        unifier <- Type.unify(impl.returnType, targetType)
      ){
        val argTypes = impl.inputTypes.map(unifier.apply)
        val newTargetType = unifier(impl.returnType)
        val arity = argTypes.length
        val costLeft = level - compCost
        if(arity==0){
          val result = impl.execute(IS(), debug = false)
          val valueVector = (0 until exampleCount).map(_ => result)
          val term = Component(compName, IS())
          state.registerTermAtLevel(level, term, newTargetType, valueVector)
        }else for(costs <- divideNumberAsSum(costLeft, arity)) {
          val candidatesForArgs = for (argIdx <- 0 until arity) yield {
            val c = costs(argIdx)
            val argType = argTypes(argIdx)
            if (!state.levelMaps(c).isTypeUnlocked(argType))
              synthesizeTypeAtLevel(c, argType)
            state.levelMaps(c)(argType)
          }
          cartesianProduct(candidatesForArgs).foreach(product => {
            val valueVector = (0 until exampleCount).map(exId => {
              val arguments = product.map(_._1(exId))
              impl.execute(arguments, debug = false)
            })
            val term = Component(compName, product.map(_._2))
            state.registerTermAtLevel(level, term, newTargetType, valueVector)
          })
        }
      }
    }

    (1 to 2).foreach(level => {
      state.openNextLevel()
      synthesizeTypeAtLevel(level, outputType)
      println(s"State at level: $level")
      state.print(exampleCount)
    })
  }


  def divideNumberAsSum(number: Int, pieces: Int): Iterator[IndexedSeq[Int]] = {
    if(pieces == 1) return Iterator(IndexedSeq(number))

    (0 to number).toIterator.flatMap(n => divideNumberAsSum(number - n, pieces - 1).map(n +: _))
  }

  def cartesianProduct[A](listOfSets: IndexedSeq[Iterable[A]]): Iterator[IndexedSeq[A]] = {
    if(listOfSets.isEmpty) return Iterator(IndexedSeq())
    listOfSets.head.toIterator.flatMap(v => cartesianProduct(listOfSets.tail).map(v +: _))
  }

}

