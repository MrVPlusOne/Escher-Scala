package escher

import escher.Term.Component

import scala.collection.mutable


object SynthesisUntyped {
  import Synthesis.{ValueVector, Input, ValueMap, GoalGraph, divideNumberAsSum, cartesianProduct}

  type ValueTermMap = mutable.Map[ValueVector, Term]

  def showValueTermMap(valueTermMap: ValueTermMap): String = {
    val compList = valueTermMap.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
    compList.mkString("{", ", ", "}")
  }

  class SynthesisState(val goalGraph: GoalGraph, var levelMaps: IndexedSeq[ValueTermMap], val totalMap: ValueTermMap) {
    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ ValueTermMap.empty
      levelMaps.length
    }

    def registerTermAtLevel(level: Int, term: Term, valueVector: ValueVector): Boolean = {
      totalMap.get(valueVector) match {
        case None =>
          totalMap(valueVector) = term
          levelMaps(level)(valueVector) = term
          true
        case Some(_) =>
          false
      }
    }

    def print(exampleCount: Int): Unit = {
      println(s"Goal: ${goalGraph.show(exampleCount)}")
      println(s"TotalMap: ${showValueTermMap(totalMap)}")
      println(s"LevelMaps:")
      levelMaps.indices.foreach{i =>
        val valueTermMap = levelMaps(i)
        val valueTermMapS = showValueTermMap(valueTermMap)
        val size = valueTermMap.size
        println(s"  $i: ($size components)\t$valueTermMapS")
      }
    }
  }


  object ValueTermMap{
    def empty: ValueTermMap = mutable.Map()
  }

  def synthesize(name: String, inputTypes: IndexedSeq[Type], inputNames: IndexedSeq[String], returnType: Type)
                (envCompMap: Map[String, ComponentImpl], compCostFunction: ComponentImpl => Int,
                 inputs: IndexedSeq[Input], outputs: IndexedSeq[TermValue])
                (decreasingArgId: Int, oracle: PartialFunction[IS[TermValue], TermValue]): Unit = {
    import DSL._

    require(inputTypes.length == inputNames.length)

    val recursiveComp = ComponentImpl(inputTypes, returnType, oracle)
    val compMap: Map[String, ComponentImpl] = envCompMap.updated(name, recursiveComp)

    def argDecrease(arg: Input, exampleId: Int) = {
      arg(decreasingArgId).valueSize < inputs(exampleId)(decreasingArgId).valueSize
    }

    val exampleCount = outputs.length
    val state = new SynthesisState(
      new GoalGraph(outputs.zipWithIndex.map(_.swap).toMap),
      IndexedSeq(),
      ValueTermMap.empty
    )

    val goalArity = inputTypes.length
    state.openNextLevel()
    state.openNextLevel()
    (0 until goalArity).foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      state.registerTermAtLevel(1, v(inputNames(argId)), valueMap)
    })

    def synthesizeTypeAtLevel(level: Int): Unit = {
      for(
        (compName, impl) <- compMap;
        compCost = compCostFunction(impl) if compCost <= level
      ){
        val arity = impl.inputTypes.length
        val costLeft = level - compCost
        if(arity==0){
          if(compCost == level) {
            val result = impl.execute(IS(), debug = false)
            val valueVector = (0 until exampleCount).map(_ => result)
            val term = Component(compName, IS())
            state.registerTermAtLevel(level, term, valueVector)
          }
        } else for(costs <- divideNumberAsSum(costLeft, arity, minNumber = 1)) {
          val candidatesForArgs = for (argIdx <- 0 until arity) yield {
            val c = costs(argIdx)
            state.levelMaps(c)
          }
          val isRecCall = compName == name
          cartesianProduct(candidatesForArgs).foreach(product => {
            val valueVector = (0 until exampleCount).map(exId => {
              val arguments = product.map(_._1(exId))
              if(isRecCall && !argDecrease(arguments, exId))
                ValueError
              else
                impl.execute(arguments, debug = false)
            })
            val term = Component(compName, product.map(_._2))
            state.registerTermAtLevel(level, term, valueVector)
          })
        }
      }
    }

    (1 to 6).foreach(level => {
      synthesizeTypeAtLevel(level)
      println(s"State at level: $level")
//      println("total components number: " + state.totalMap.size)
      state.print(exampleCount)
      state.openNextLevel()
    })
  }












}
