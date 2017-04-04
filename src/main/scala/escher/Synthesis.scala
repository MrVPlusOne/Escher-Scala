package escher

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

    def registerTermAtLevel(level: Int, term: Term, ty: Type, valueMap: ValueVector): Boolean = {
      totalMap(ty).get(valueMap) match {
        case None =>
          totalMap.registerTerm(term, ty, valueMap)
          levelMaps(level).registerTerm(term, ty, valueMap)
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
    def apply(ty: Type): Map[ValueVector, Term] = {
      map.getOrElse(ty, Map())
    }

    def registerTerm(term: Term, ty: Type, valueMap: ValueVector): Unit = {
      val v2t = apply(ty)
      map(ty) = v2t.updated(valueMap, term)
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
                (compMap: Map[String, ComponentImpl], inputs: IndexedSeq[Input], outputs: IndexedSeq[TermValue]) = {
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

    def synthesizeTypeAtLevel(level: Int, targetType: Type): Unit ={

    }


//    def synUnderCost(maxCost: Int, targetType: Type): Unit ={
//      require(maxCost >= 0)
//      if(maxCost == 0){
//        if(state.levelMaps)
//      }
//    }
    state.print(exampleCount)
  }
}

