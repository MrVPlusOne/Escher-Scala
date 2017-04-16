package escher

import escher.Term.Component

import scala.collection.mutable
import SynthesisUntyped._
import escher.ImmutableGoalGraph.GoalManager
import escher.Synthesis._


object SynthesisUntyped {
  case class Config(
                     maxCost: Int,
                     printComponents: Boolean = true,
                     printLevels: Boolean = true,
                     printAtReboot: Boolean = true,
                     rebootStrategy: RebootStrategy = RebootStrategy.addSimplestFailedExample,
                     argListCompare: (ArgList, ArgList) => Boolean = ArgList.anyArgSmaller
                   )
}



class SynthesisUntyped(config: Config, logger: String => Unit) {

  def logLn(msg: String): Unit = {
    logger(msg)
    logger("\n")
  }

  class SynthesisState(val examples: IS[(ArgList,TermValue)], val bufferedOracle: BufferedOracle, val totalMap: ValueTermMap) {
    private var levelMaps: IS[ValueTermMap] = IS()

    def getLevelOfCost(cost: Int): ValueTermMap = levelMaps(cost-1)

    def library(vm: ValueMap): Option[Term] = {
      levelMaps.foreach(map => {
        map.searchATerm(vm).foreach(t => return Some(t))
      })

      None
    }

    val manager: GoalManager = {
      val initGoal = bufferedOracle.examples.map(_._2)
      new GoalManager(
        initGoal = initGoal.zipWithIndex.map(_.swap).toMap,
        boolLibrary = library,
        valueLibrary = library,
        exampleCount = initGoal.length,
        printer = (n, s) => logger("  " * n + s)
      )
    }

    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ new ValueVectorTree(examples.length)
      levelMaps.length
    }

    /**
      * Register a new term into this state, then update the goal graph accordingly
      *
      * @return Whether the root goal has been solved
      */
    def registerTermAtLevel(cost: Int, term: Term, valueVector: ValueVector): Boolean = {
      totalMap.get(valueVector) match {
        case None =>
          manager.insertNewTerm(valueVector, term)
          totalMap(valueVector) = term
          getLevelOfCost(cost)(valueVector) = term
          manager.root.isSolved
        case Some(_) =>
          false
      }
    }

    def print(exampleCount: Int): Unit = {
      manager.printState()
      logLn(s"TotalMap: (${totalMap.size} components in total)")
      if(config.printComponents) {
        logLn("  " + showValueTermMap(totalMap))
      }
      if(config.printLevels) {
        logLn(s"LevelMaps:")
        levelMaps.indices.foreach { i =>
          val valueTermMap = levelMaps(i)
          val size = valueTermMap.size
          logLn(s"  ${i+1}: ($size components)")
          if (config.printComponents) {
            val valueTermMapS = showValueTermMap(valueTermMap)
            logLn(s"      $valueTermMapS")
          }
        }
      }
    }
  }




  def synthesize(name: String, inputTypes: IS[Type], inputNames: IS[String], returnType: Type)
                (envCompMap: Map[String, ComponentImpl],
                 compCostFunction: (ComponentImpl) => Int,
                 examples: IS[(ArgList, TermValue)],
                 oracle: PartialFunction[IS[TermValue], TermValue],
                 oracleBuffer: IS[(ArgList, TermValue)] = IS()): Option[(SynthesizedComponent, SynthesisState)] = {
    import DSL._

    val inputs: IS[ArgList] = examples.map(_._1)
    val outputs: IS[TermValue] = examples.map(_._2)


    require(inputTypes.length == inputNames.length)

    val bufferedOracle = new BufferedOracle(examples, oracle, oracleBuffer)
    val recursiveComp = ComponentImpl(inputTypes, returnType, PartialFunction(bufferedOracle.evaluate))
    val compMap: Map[String, ComponentImpl] = envCompMap.updated(name, recursiveComp)

    def argDecrease(arg: ArgList, exampleId: Int) = {
      config.argListCompare(arg, inputs(exampleId))
    }

    val exampleCount = outputs.length
    val state = new SynthesisState(
      examples,
      bufferedOracle,
      new ValueVectorTree(exampleCount)
    )

    def resultFromState(): Option[(SynthesizedComponent, SynthesisState)] = {
      val body = state.manager.synthesizedProgram
      val impl = ComponentImpl.recursiveImpl(name, inputNames, inputTypes, returnType,
        envCompMap, config.argListCompare, body)
      var passed, failed = IS[(ArgList, TermValue)]()
      bufferedOracle.buffer.foreach{
        case (a,r) =>
          if(impl.execute(a, debug = false) == r)
            passed = passed :+ (a -> r)
          else
            failed = failed :+ (a -> r)
      }
      val comp = SynthesizedComponent(name, inputNames, inputTypes, returnType, body)
      if(failed.isEmpty){
        Some((comp, state))
      }else {
        if(config.printAtReboot){
          logLn("--- Reboot ---")
          logLn(s"Program Found:")
          logLn(comp.show)
          logLn(s"which failed at ${failed.map{
            case (a,r) => s"${ArgList.showArgList(a)} -> ${r.show}"
          }.mkString("; ")}")
          logLn("Now Reboot...")
        }

        val (newExamples, newBuffer) = config.rebootStrategy.newExamplesAndOracleBuffer(examples, failed, passed)
        synthesize(name, inputTypes, inputNames, returnType)(
          envCompMap, compCostFunction, newExamples, oracle, newBuffer
        )
      }
    }

    state.openNextLevel()
    inputTypes.indices.foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      if(state.registerTermAtLevel(1, v(inputNames(argId)), valueMap)){
        return resultFromState()
      }
    })

    /**
      * @return Whether the goal has been solved
      */
    def synthesizeAtLevel(level: Int): Boolean = {
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
            if(state.registerTermAtLevel(level, term, valueVector))
              return true
          }
        } else for(costs <- divideNumberAsSum(costLeft, arity, minNumber = 1)) {
          val candidatesForArgs = for (argIdx <- 0 until arity) yield {
            val c = costs(argIdx)
            state.getLevelOfCost(c).root.toIndexedSeq
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
            if(state.registerTermAtLevel(level, term, valueVector))
              return true
          })
        }
      }
      false
    }

    (1 to config.maxCost).foreach(level => {
      if(synthesizeAtLevel(level))
        return resultFromState()
      logLn(s"State at level: $level")
//      println("total components number: " + state.totalMap.size)
      state.print(exampleCount)
      state.openNextLevel()
    })
    None
  }












}
