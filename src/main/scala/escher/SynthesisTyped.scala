package escher


import escher.SynthesisTyped._
import escher.Term.Component

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
                     logTotalMap: Boolean = true
                   )
}

class SynthesisTyped(config: Config, logger: String => Unit) {
  import Synthesis.{ValueVector, Input, divideNumberAsSum, cartesianProduct, ValueTermMap, notAllErr}

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

  class SynthesisState(initGoal: ValueVector, val totalMap: TypeMap) {
    private var levelMaps: IS[TypeMap] = IS()

    def getLevelOfCost(cost: Int): TypeMap = levelMaps(cost-1)

    def openNextLevel(): Int ={
      levelMaps = levelMaps :+ TypeMap.empty
      levelMaps.length
    }

    def openToLevel(n: Int): Unit ={
      (0 to n - levelMaps.length).foreach(_ => openNextLevel())
    }

    def registerTermAtLevel(cost: Int, ty: Type, term: Term, valueVector: ValueVector): Boolean = {
      val ty1 = Type.alphaNormalForm(ty)
      totalMap(ty1).get(valueVector) match {
        case None =>
          totalMap(ty1)(valueVector) = term
          getLevelOfCost(cost)(ty1)(valueVector) = term
          true
        case Some(_) =>
          false
      }
    }

    def print(exampleCount: Int): Unit = {
//      logLn(config.logGoal)(s"Goal: ${goalGraph.show(exampleCount)}")
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


  def synthesize(name: String, inputTypes: IndexedSeq[Type], inputNames: IndexedSeq[String], returnType: Type)
                (decreasingArgId: Int, oracle: PartialFunction[IS[TermValue], TermValue])
                (envCompMap: Map[String, ComponentImpl], compCostFunction: ComponentImpl => Int,
                 inputs: IndexedSeq[Input], outputs: IndexedSeq[TermValue]): Unit = {
    import DSL._

    require(inputTypes.length == inputNames.length)

    val recursiveComp = ComponentImpl(inputTypes, returnType, oracle)
    val compMap: Map[String, ComponentImpl] = envCompMap.updated(name, recursiveComp)

    def argDecrease(arg: Input, exampleId: Int) = {
      arg(decreasingArgId) smallerThan inputs(exampleId)(decreasingArgId)
    }

    val exampleCount = outputs.length
    val state = new SynthesisState(
      outputs,
      TypeMap.empty()
    )

    val goalArity = inputTypes.length
    state.openNextLevel()
    (0 until goalArity).foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      state.registerTermAtLevel(1, inputTypes(argId), v(inputNames(argId)), valueMap)
    })

    def synthesizeAtLevel(level: Int): Unit = {
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
            state.registerTermAtLevel(level, impl.returnType, term, valueVector)
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
              state.registerTermAtLevel(level, returnType, term, valueVector)
            }
          })
        }
      }
    }

    (1 to config.maxCost).foreach(level => {
      synthesizeAtLevel(level)
      logger(s"State at level: $level\n")
      //      println("total components number: " + state.totalMap.size)
      state.print(exampleCount)
      state.openNextLevel()
    })
  }

  def typesForCosts(state: SynthesisState, costs: IS[Int],
                    inputTypes: IS[Type], returnType: Type): Iterator[(IS[Type], Type)] = {
    val signatureNextFreeId =  (returnType.nextFreeId +: inputTypes.map(_.nextFreeId)).max

    //fixme to past tests
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

