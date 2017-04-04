package escher

import scala.collection.mutable


/**
  * The program synthesizing algorithm
  */
object Synthesis {
  type ValueMap = Map[Int, TermValue]
  type Input = IndexedSeq[TermValue]

  object ValueMap{
    def show(valueMap: ValueMap, exampleCount: Int): String = {
      (0 until exampleCount).map(i => valueMap.get(i).map(_.show).getOrElse("?")).mkString("<", ", ", ">")
    }
  }


  class SynthesisState(val goalGraph: GoalGraph, var levelMaps: IndexedSeq[TypeMap], val totalMap: TypeMap) {
    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ TypeMap.empty
      levelMaps.length
    }

    def registerTermAt(level: Int, term: Term, ty: Type, valueMap: ValueMap): Boolean = {
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
      println(s"TotalMap: ${totalMap.show(exampleCount)}")
      val levelsShow = levelMaps.map(_.show(exampleCount)).zipWithIndex.map{
        case (s, level) => s"  $level: $s"
      }.mkString("\n")
      println(s"LevelMaps:\n $levelsShow")
    }

  }

  class TypeMap private(private val map: mutable.Map[Type, Map[ValueMap, Term]]){
    def apply(ty: Type): Map[ValueMap, Term] = {
      map.getOrElse(ty, Map())
    }

    def registerTerm(term: Term, ty: Type, valueMap: ValueMap): Unit = {
      val v2t = apply(ty)
      map(ty) = v2t.updated(valueMap, term)
    }

    def show(exampleCount: Int): String = {
      map.mapValues{map =>
        val compList = map.map{case (vMap, term) => s"'${term.show}': ${ValueMap.show(vMap, exampleCount)}"}
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
        exId -> inputs(exId)(argId)
      }).toMap
      state.registerTermAt(0, v(inputNames(argId)), inputTypes(argId), valueMap)
    })


//    def synUnderCost(maxCost: Int, targetType: Type): Unit ={
//      require(maxCost >= 0)
//      if(maxCost == 0){
//        if(state.levelMaps)
//      }
//    }
    state.print(exampleCount)
  }
}

