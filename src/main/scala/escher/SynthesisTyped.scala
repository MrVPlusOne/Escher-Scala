package escher


import SynthesisTyped._
import Term.Component
import Synthesis._
import escher.ImmutableGoalGraph.GoalManager

import scala.collection.mutable

object SynthesisTyped{
  /**
    *
    * @param deleteAllErr whether to delete synthesized components whose value vector consists only of Err
    */
  case class Config(
                     maxCost: Int = Int.MaxValue,
                     deleteAllErr: Boolean = true,
                     logGoal: Boolean = true,
                     logLevels: Boolean = true,
                     logComponents: Boolean = true,
                     logTotalMap: Boolean = true,
                     logReboot: Boolean = true,
                     rebootStrategy: RebootStrategy = RebootStrategy.addSimplestFailedExample,
                     argListCompare: (ArgList, ArgList) => Boolean = ArgList.anyArgSmaller
                   )

  object ValueTermMap{
    def empty(depth: Int): ValueTermMap = new ValueVectorTree(depth)
  }

  case class SynthesisData(oracleBuffer: IS[(ArgList, TermValue)], reboots: Int)

  object SynthesisData{
    def init: SynthesisData = SynthesisData(oracleBuffer = IS(), reboots = 0)
  }

  def printResult(syn: SynthesisTyped)
                 (result: Option[(SynthesizedComponent, syn.SynthesisState, SynthesisData)]): Unit = {
    result match {
      case Some((program, state, synData)) =>
        val examples = state.examples
        println(s"------ Synthesis for ${program.name} Succeeded! (${synData.reboots} reboots) ------")
        println(s"Initial examples (${examples.length}):")
        examples.foreach { case (a, r) =>
          print(ArgList.showArgList(a))
          print(" -> ")
          println(r.show)
        }
        println(s"Additional examples provided (${synData.oracleBuffer.length}):")
        synData.oracleBuffer.foreach { case (a, r) =>
          print(ArgList.showArgList(a))
          print(" -> ")
          println(r.show)
        }
        state.print(exampleCount = examples.length)
        println(s"\nProgram found:\n")
        program.print()
      case _ =>
        println(s"------- Synthesis Failed. -------")
    }
  }

}

class SynthesisTyped(config: Config, logger: String => Unit) {

  def log(condition: Boolean)(msg: =>String): Unit = {
    if(condition)
      logger(msg)
  }
  def logLn(condition: Boolean)(msg: =>String): Unit = {
    if(condition){
      logger(msg)
      logger("\n")
    }
  }

  class TypeMap private(private val map: mutable.Map[Type, ValueTermMap], examples: Int){

    def apply(ty: Type): ValueTermMap = {
      val v = map.getOrElse(ty, ValueTermMap.empty(examples))
      map(ty) = v
      v
    }

    def registerTerm(term: Term, ty: Type, valueVector: ValueVector): Unit = {
      apply(ty).addTerm(term, valueVector)
    }

    def show: String = {
      map.mapValues{map =>
        val compList = map.elements.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
        compList.mkString("{", ", ", "}")
      }.toString
    }

    def print(indentation: Int): Unit = {
      val whiteSpace = " " * indentation
      map.mapValues{map =>
        val compList = map.elements.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
        compList.mkString("{", ", ", "}")
      }.foreach{
        case (k, v) =>
          logger(whiteSpace)
          logger(s"(${map(k).size}) $k  -> $v")
          logger("\n")
      }
    }

    def typesIterator: Iterator[Type] = map.keysIterator

    def statString: String = s"${map.values.map(_.size).sum} components, ${map.keys.size} types"
  }
  object TypeMap{
    def empty(depth: Int) = new TypeMap(mutable.Map(), depth)
  }

  class SynthesisState(val examples: IS[(ArgList,TermValue)], val totalMap: TypeMap, returnType: Type) {
    import DSL._

    val targetType: Type = Type.alphaNormalForm(returnType)

    private var levelMaps: IS[TypeMap] = IS()

    def getLevelOfCost(cost: Int): TypeMap = levelMaps(cost-1)

    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ TypeMap.empty(examples.length)
      levelMaps.length
    }

    def library(ty: Type)(vm: ValueMap): Option[Term] = {
      levelMaps.foreach(map => {
        map(ty).searchATerm(vm).foreach(t => return Some(t))
      })

      None
    }

    def libraryOfCost(ty: Type)(cost: Int, vm: ValueMap): Option[Term] = {
      val map = getLevelOfCost(cost)
      map(ty).searchATerm(vm)
    }

    private val termsOfCostBuffer = mutable.Map[Int, IS[(ValueVector, Term)]]()
    def termsOfCost(cost: Int): IS[(ValueVector, Term)] = {
      termsOfCostBuffer.getOrElse(cost, {
        val is = getLevelOfCost(cost)(returnType).elements.toIndexedSeq
        termsOfCostBuffer(cost) = is
        is
      })
    }

    def openToLevel(n: Int): Unit ={
      (0 to n - levelMaps.length).foreach(_ => openNextLevel())
    }

    /**
      * Register a new term into this state, then update the goal graph accordingly
      *
      * @return Whether the root goal has been solved
      */
    def registerTermAtLevel(cost: Int, ty: Type, term: Term, valueVector: ValueVector): Boolean = {
      val ty1 = Type.alphaNormalForm(ty)

      totalMap.typesIterator.foreach(t => {
        if((ty1 instanceOf t) && totalMap(t).get(valueVector).nonEmpty)
          return false
      })

      totalMap(ty1)(valueVector) = term
      getLevelOfCost(cost)(ty1)(valueVector) = term
      false
    }

    def print(exampleCount: Int): Unit = {
      logLn(config.logTotalMap)(s"TotalMap: (${totalMap.statString})")
      if(config.logTotalMap && config.logComponents){
        totalMap.print(4)
      }
      logLn(config.logLevels)(s"LevelMaps:")
      if(config.logLevels)
        levelMaps.indices.foreach{i =>
          val c = i + 1
          val typeMap = getLevelOfCost(c)
          logLn(condition = true)(s"  $c: (${typeMap.statString})")
          if(config.logComponents)
            typeMap.print(6)
        }
    }
  }


  def synthesize(name: String, inputTypesFree: IndexedSeq[Type], inputNames: IndexedSeq[String], returnTypeFree: Type)
                (envCompMap: Map[String, ComponentImpl], compCostFunction: (ComponentImpl) => Int,
                 examples: IS[(ArgList, TermValue)],
                 oracle: PartialFunction[IS[TermValue], TermValue],
                 synData: SynthesisData = SynthesisData.init): Option[(SynthesizedComponent, SynthesisState, SynthesisData)] = {
    import DSL._

    val inputs: IS[ArgList] = examples.map(_._1)
    val outputs: IS[TermValue] = examples.map(_._2)
    val inputTypes = inputTypesFree.map(_.fixVars)
    val returnType = returnTypeFree.fixVars

    require(inputTypes.length == inputNames.length)

    val bufferedOracle = new BufferedOracle(inputs.zip(outputs), oracle, initBuffer = synData.oracleBuffer)
    val recursiveComp = ComponentImpl(inputTypes, returnType, PartialFunction(bufferedOracle.evaluate))
    val compMap: Map[String, ComponentImpl] = envCompMap.updated(name, recursiveComp)

    def argDecrease(arg: ArgList, exampleId: Int) = {
      config.argListCompare(arg, inputs(exampleId))
    }

    val exampleCount = outputs.length
    val state = new SynthesisState(
      examples,
      TypeMap.empty(exampleCount),
      returnType
    )

    def resultFromState(term: Term): Option[(SynthesizedComponent, SynthesisState, SynthesisData)] = {
      val body = term
      val comp = SynthesizedComponent(name, inputNames, inputTypes, returnType, body)
      logLn(config.logReboot){
        s"Program Found:\n${comp.show}"
      }
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
      if(failed.isEmpty){
        Some((comp, state, synData))
      }else {
        if(config.logReboot){
          println("--- Reboot ---")
        }
        logLn(config.logReboot){
          s"""
            |  which failed at ${showExamples(failed)}
            |Now Reboot...
          """.stripMargin
        }

        val (newExamples, newBuffer) = config.rebootStrategy.newExamplesAndOracleBuffer(examples, failed, passed)
        println(s"New examples: ${showExamples(newExamples)}")
        val newSynData = SynthesisData(newBuffer, synData.reboots+1)
        synthesize(name, inputTypes, inputNames, returnType)(
          envCompMap, compCostFunction, newExamples, oracle, newSynData
        )
      }
    }

    state.openNextLevel()
    inputTypes.indices.foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      state.registerTermAtLevel(1, inputTypes(argId), v(inputNames(argId)), valueMap)
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
            if(state.registerTermAtLevel(level, impl.returnType, term, valueVector))
              return true
          }
        } else for(
          costs <- divideNumberAsSum(costLeft, arity, minNumber = 1);
          (argTypes, returnType) <- typesForCosts(state, costs, impl.inputTypes, impl.returnType)
        ) {
          val candidatesForArgs =
            for (argIdx <- 0 until arity) yield {
              val c = costs(argIdx)
              state.getLevelOfCost(c)(argTypes(argIdx)).elements
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
            if(!config.deleteAllErr || notAllErr(valueVector)) {
              val term = Component(compName, product.map(_._2))
              if(state.registerTermAtLevel(level, returnType, term, valueVector))
                return true
            }
          })
        }
      }
      false
    }



    val goalVM = outputs.zipWithIndex.map(_.swap).toMap
    (1 to config.maxCost).foreach(level => {
      synthesizeAtLevel(level)
      val search = new BatchGoalSearch(
        termOfCostAndVM = state.libraryOfCost(returnType),
        termsOfCost = state.termsOfCost,
        boolOfVM = state.library(tyBool)
      )
      search.search(level, goalVM).foreach{term =>
        return resultFromState(term)
      }

      logger(s"State at level: $level\n")
      state.print(exampleCount)
      state.openNextLevel()
    })
    None
  }

  def typesForCosts(state: SynthesisState, costs: IS[Int],
                    inputTypes: IS[Type], returnType: Type): Iterator[(IS[Type], Type)] = {
    val signatureNextFreeId =  (returnType.nextFreeId +: inputTypes.map(_.nextFreeId)).max

    def aux(argId: Int, nextFreeId: Int, subst: TypeSubst): Iterator[(IS[Type], Type)] = {
      if(argId == costs.length) return Iterator((IS(), Type.alphaNormalForm(subst(returnType))))

      val c = costs(argId)
      val requireType = subst(inputTypes(argId))
      state.getLevelOfCost(c).typesIterator.flatMap { t =>
        val candidateType = t.shiftId(nextFreeId)
        val unifyResult = Type.unify(requireType, candidateType)
        unifyResult match {
          case Some(unifier) =>
            val newFreeId = nextFreeId + t.nextFreeId
            aux(argId+1, newFreeId, subst.compose(unifier)).map {
              case (is, r) => (t +: is, r)
            }
          case None =>
            Iterator()
        }
      }
    }

    aux(0, signatureNextFreeId, TypeSubst.empty)
  }


}


