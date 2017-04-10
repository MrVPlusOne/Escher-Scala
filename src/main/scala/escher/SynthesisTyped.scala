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
                     logReboot: Boolean = true
                   )
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

  class TypeMap private(private val map: mutable.Map[Type, ValueTermMap]){

    def apply(ty: Type): ValueTermMap = {
      val v = map.getOrElse(ty, mutable.Map())
      map(ty) = v
      v
    }

    def registerTerm(term: Term, ty: Type, valueMap: ValueVector): Unit = {
      apply(ty).update(valueMap, term)
    }

    def show: String = {
      map.mapValues{map =>
        val compList = map.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
        compList.mkString("{", ", ", "}")
      }.toString
    }

    def print(indentation: Int): Unit = {
      val whiteSpace = " " * indentation
      map.mapValues{map =>
        val compList = map.map{case (vMap, term) => s"'${term.show}': ${ValueVector.show(vMap)}"}
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
    def empty() = new TypeMap(mutable.Map())
  }

  class SynthesisState(val examples: IS[(ArgList,TermValue)], val totalMap: TypeMap, returnType: Type) {
    import DSL._

    val targetType: Type = Type.alphaNormalForm(returnType)

    private var levelMaps: IS[TypeMap] = IS()

    def getLevelOfCost(cost: Int): TypeMap = levelMaps(cost-1)

    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ TypeMap.empty
      levelMaps.length
    }

    def library(ty: Type)(vm: ValueMap): Option[Term] = {
      for((vec,term) <- totalMap(ty)){
        if(ValueMap.matchVector(vm, vec))
          return Some(term)
      }
      None
    }

    val manager: GoalManager = {
      val initGoal = examples.map(_._2)
      new GoalManager(
        initGoal = initGoal.zipWithIndex.map(_.swap).toMap,
        boolLibrary = library(tyBool),
        valueLibrary = library(targetType),
        exampleCount = initGoal.length,
        printer = (n, s) => logger("  "*n + s)
      )
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
      totalMap(ty1).get(valueVector) match {
        case None =>
          if(ty1 == tyBool || ty1 == targetType) //todo: deal with tyBool specially
            manager.insertNewTerm(valueVector, term)
          totalMap(ty1)(valueVector) = term
          getLevelOfCost(cost)(ty1)(valueVector) = term
          manager.root.isSolved
        case Some(_) =>
          false
      }
    }

    def print(exampleCount: Int): Unit = {
      if(config.logGoal)
        manager.printState()
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


  object ValueTermMap{
    def empty: ValueTermMap = mutable.Map()
  }


  def synthesize(name: String, inputTypesFree: IndexedSeq[Type], inputNames: IndexedSeq[String], returnTypeFree: Type)
                (envCompMap: Map[String, ComponentImpl], compCostFunction: (ComponentImpl) => Int,
                 examples: IS[(ArgList, TermValue)],
                 oracle: PartialFunction[IS[TermValue], TermValue]): Option[(SynthesizedComponent, SynthesisState, BufferedOracle)] = {
    import DSL._

    val inputs: IS[ArgList] = examples.map(_._1)
    val outputs: IS[TermValue] = examples.map(_._2)
    val inputTypes = inputTypesFree.map(_.fixVars)
    val returnType = returnTypeFree.fixVars

    require(inputTypes.length == inputNames.length)

    val bufferedOracle = new BufferedOracle(inputs.zip(outputs), oracle)
    val recursiveComp = ComponentImpl(inputTypes, returnType, PartialFunction(bufferedOracle.evaluate))
    val compMap: Map[String, ComponentImpl] = envCompMap.updated(name, recursiveComp)

    def argDecrease(arg: ArgList, exampleId: Int) = {
      ArgList.alphabeticSmallerThan(arg, inputs(exampleId))
    }

    val exampleCount = outputs.length
    val state = new SynthesisState(
      examples,
      TypeMap.empty(),
      returnType
    )

    def resultFromState(): Option[(SynthesizedComponent, SynthesisState, BufferedOracle)] = {
      val body = state.manager.synthesizedProgram
      val comp = SynthesizedComponent(name, inputNames, inputTypes, returnType, body)
      logLn(config.logReboot){
        s"Program Found:\n${comp.show}"
      }
      val impl = ComponentImpl.recursiveImpl(name, inputNames, inputTypes, returnType, envCompMap, body)
      var passed, failed = IS[(ArgList, TermValue)]()
      bufferedOracle.buffer.foreach{
        case (a,r) =>
          if(impl.execute(a, debug = false) == r)
            passed = passed :+ (a -> r)
          else
            failed = failed :+ (a -> r)
      }
      if(failed.isEmpty){
        Some((comp, state, bufferedOracle))
      }else {
        //todo: add more reboot strategy
        if(config.logReboot){
          println("--- Reboot ---")
          state.manager.printState()
        }
        logLn(config.logReboot){
          s"""
            |  which failed at ${failed.map{ case (a,r) => s"${ArgList.showArgList(a)} -> ${r.show}" }.mkString("; ")}
            |Now Reboot...
          """.stripMargin
        }

        val newExamples = examples ++ failed ++ passed
        synthesize(name, inputTypes, inputNames, returnType)(
          envCompMap, compCostFunction, newExamples, oracle
        )
      }
    }

    state.openNextLevel()
    inputTypes.indices.foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      if(state.registerTermAtLevel(1, inputTypes(argId), v(inputNames(argId)), valueMap))
        return resultFromState()
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
              state.getLevelOfCost(c)(argTypes(argIdx))
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

    (1 to config.maxCost).foreach(level => {
      if(synthesizeAtLevel(level))
        return resultFromState()
      logger(s"State at level: $level\n")
      //      println("total components number: " + state.totalMap.size)
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

