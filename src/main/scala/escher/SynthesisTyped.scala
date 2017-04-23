package escher


import SynthesisTyped._
import Term.Component
import Synthesis._

import scala.collection.mutable

object SynthesisTyped{
  /**
    *
    * @param deleteAllErr whether to delete synthesized components whose value vector consists only of Err
    * @param searchSizeFactor searchSizeFactor = maxProgramCost / level
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
                     argListCompare: (ArgList, ArgList) => Boolean = ArgList.anyArgSmaller,
                     searchSizeFactor: Int = 3
                   )


  case class SynthesisData(oracleBuffer: IS[(ArgList, TermValue)], reboots: Int, lastRebootTimeUsed: TimeTools.Nanosecond)

  object SynthesisData{
    def init: SynthesisData = SynthesisData(oracleBuffer = IS(), reboots = 0, lastRebootTimeUsed = 0)
  }

  def printResult(syn: SynthesisTyped, maxExamplesShown: Int = 9)
                 (result: Option[(SynthesizedComponent, syn.SynthesisState, SynthesisData)]): Unit = {
    def showExamples(tag: String, examples: IS[(ArgList, TermValue)], maxExamplesShown: Int): Unit ={
      println(s"$tag (${examples.length}):")
      examples.take(maxExamplesShown).foreach { case (a, r) =>
        print(ArgList.showArgList(a))
        print(" -> ")
        println(r.show)
      }
      if(examples.length > maxExamplesShown)
        println(s" ...(${examples.length - maxExamplesShown} more not shown)...")
    }


    result match {
      case Some((program, state, synData)) =>
        val examples = state.examples
        println(s"------ Synthesis for ${program.name} Succeeded! (${synData.reboots} reboots) ------")
        println(s"Time used for the last reboot: ${TimeTools.nanoToMillisString(synData.lastRebootTimeUsed)}")
        showExamples("Initial Examples", examples, maxExamplesShown = 50)
        showExamples("Additional examples provided", synData.oracleBuffer, maxExamplesShown)
        state.print(exampleCount = examples.length)
        println(s"\nProgram found:\n")
        program.print()
        println()
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
      val v = map.getOrElse(ty, ValueTermMap.empty())
      map(ty) = v
      v
    }

    def get(ty: Type): Option[ValueTermMap] = map.get(ty)

    def registerTerm(term: Term, ty: Type, valueVector: ValueVector): Unit = {
      apply(ty)(valueVector) = term
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
    def empty(depth: Int) = new TypeMap(mutable.Map(), depth)
  }

  class SynthesisState(val examples: IS[(ArgList,TermValue)], val totalMap: TypeMap, returnType: Type) {
    import DSL._

    private var _levelMaps: IS[TypeMap] = IS()

    def levels: Int = _levelMaps.length

    def getLevelOfCost(cost: Int): TypeMap = _levelMaps(cost-1)

    def openNextLevel(): Int ={
      _levelMaps = _levelMaps :+ TypeMap.empty(examples.length)
      _levelMaps.length
    }

    private var returnTypeVectorTrees: IS[ValueVectorTree[Term]] = IS()
    private var boolVectorTrees: IS[ValueVectorTree[Term]] = IS()

    def createLibrariesForThisLevel(): Unit ={
      require(returnTypeVectorTrees.length == levels - 1)
      require(boolVectorTrees.length == levels - 1)

      val returnTypeTree = new ValueVectorTree[Term](examples.length)
      val typeMap = getLevelOfCost(levels)
      for(ty <- typesMatch(typeMap, returnType)){
        val vt = typeMap(ty)
        vt.foreach{ case (vv, term) => returnTypeTree.addTerm(term, vv) }
      }
      returnTypeVectorTrees = returnTypeVectorTrees :+ returnTypeTree

      val boolTree = new ValueVectorTree[Term](examples.length)
      for(ty <- typesMatch(typeMap, tyBool)){
        val vt = typeMap(ty)
        vt.foreach{ case (vv, term) => boolTree.addTerm(term, vv) }
      }
      boolVectorTrees = boolVectorTrees :+ boolTree
    }

    private def typesMatch(typeMap: TypeMap, ty: Type): List[Type] = {
      typeMap.typesIterator.collect{
        case t if ty instanceOf t =>
          Type.alphaNormalForm(t)
      }.toList
    }

    def boolLibrary(vm: IndexValueMap): Option[(Int, Term)] = {
      for(cost <- 1 to levels){
        val tree = boolVectorTrees(cost-1)
        tree.searchATerm(vm).foreach(t => return Some(cost -> t))
      }
      None
    }

    def returnTypeLibrary(vm: IndexValueMap): Option[(Int, Term)] = {
      for(cost <- 1 to levels){
        val tree = returnTypeVectorTrees(cost-1)
        tree.searchATerm(vm).foreach(t => return Some(cost -> t))
      }
      None
    }

    def libraryOfCost(cost: Int, vm: IndexValueMap): Option[Term] = {
      val tree = returnTypeVectorTrees(cost-1)
      tree.searchATerm(vm)
    }

    def termsOfCost(cost: Int): Iterable[(ValueVector, Term)] = {
      returnTypeVectorTrees(cost-1).elements
    }

    def openToLevel(n: Int): Unit ={
      (0 to n - _levelMaps.length).foreach(_ => openNextLevel())
    }

    /**
      * Register a new term into this state, then update the goal graph accordingly
      *
      * @return Whether the root goal has been solved
      */
    def registerTermAtLevel(cost: Int, ty: Type, term: Term, valueVector: ValueVector): Boolean = {
      val ty1 = Type.alphaNormalForm(ty)

      if(totalMap(ty1).get(valueVector).nonEmpty)
        return false

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
        _levelMaps.indices.foreach{i =>
          val c = i + 1
          val typeMap = getLevelOfCost(c)
          logLn(condition = true)(s"  $c: (${typeMap.statString})")
          if(config.logComponents)
            typeMap.print(6)
        }
    }
  }


  def synthesize(name: String, inputTypesFree: IndexedSeq[Type], inputNames: IndexedSeq[String], returnTypeFree: Type)
                (envCompMap: Map[String, ComponentImpl], examples0: IS[(ArgList, TermValue)], oracle: PartialFunction[IS[TermValue], TermValue], synData: SynthesisData = SynthesisData.init): Option[(SynthesizedComponent, SynthesisState, SynthesisData)] = {
    import DSL._

    val startTime = System.nanoTime()

    val examples = examples0.sortWith(Synthesis.exampleLt)
    val inputs: IS[ArgList] = examples.map(_._1)
    val outputs: IS[TermValue] = examples.map(_._2)
    val inputTypes = inputTypesFree.map(_.fixVars)
    val goalReturnType = returnTypeFree.fixVars

    require(inputTypes.length == inputNames.length)

    val bufferedOracle = new BufferedOracle(inputs.zip(outputs), oracle, initBuffer = synData.oracleBuffer)
    val recursiveComp = ComponentImpl(inputTypes, goalReturnType, PartialFunction(bufferedOracle.evaluate))
    val compMap: Map[String, ComponentImpl] = envCompMap.updated(name, recursiveComp)

    def argDecrease(arg: ArgList, exampleId: Int) = {
      config.argListCompare(arg, inputs(exampleId))
    }

    val exampleCount = outputs.length
    val state = new SynthesisState(
      examples,
      TypeMap.empty(exampleCount),
      goalReturnType
    )

    def resultFromState(cost:Int, depth: Int, term: Term): Option[(SynthesizedComponent, SynthesisState, SynthesisData)] = {
      val body = term
      val comp = SynthesizedComponent(name, inputNames, inputTypes, goalReturnType, body, cost, depth)
      val impl = ComponentImpl.recursiveImpl(name, inputNames, inputTypes, goalReturnType,
        envCompMap, config.argListCompare, body)
      var passed, failed = IS[(ArgList, TermValue)]()
      bufferedOracle.buffer.foreach{
        case (a,r) =>
          if(impl.executeEfficient(a) == r)
            passed = passed :+ (a -> r)
          else
            failed = failed :+ (a -> r)
      }
      if(failed.isEmpty){
        val timeUse = System.nanoTime() - startTime
        Some((comp, state, synData.copy(oracleBuffer = passed, lastRebootTimeUsed = timeUse)))
      }else {
        if(config.logReboot){
          println(s"Failed program found:")
          comp.print()
        }
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
        val newSynData = SynthesisData(newBuffer, synData.reboots+1, lastRebootTimeUsed = 0)
        synthesize(name, inputTypes, inputNames, goalReturnType)(envCompMap, newExamples, oracle, newSynData)
      }
    }

    def compCostFunction(impl: ComponentImpl): Int = {
      1
    }


    val isInterestingSignature = Synthesis.isInterestingSignature(goalReturnType, inputTypes)

    def synthesizeAtLevel(cost: Int, synBoolAndReturnType: Boolean): Unit = {
      for(
        (compName, impl) <- compMap;
        compCost = compCostFunction(impl) if compCost <= cost
      ){
        val arity = impl.inputTypes.length
        val costLeft = cost - compCost
        if(arity==0){
          if(compCost == cost) {
            val result = impl.executeEfficient(IS())
            val valueVector = (0 until exampleCount).map(_ => result)
            val term = Component(compName, IS())
            state.registerTermAtLevel(cost, impl.returnType, term, valueVector)
          }
        } else for(
          costs <- divideNumberAsSum(costLeft, arity, minNumber = 1);
          (argTypes, returnType) <- typesForCosts(c => state.getLevelOfCost(c).typesIterator, costs, impl.inputTypes, impl.returnType)
          if (synBoolAndReturnType == (goalReturnType.instanceOf(returnType) || tyBool.instanceOf(returnType))) &&
            isInterestingSignature(argTypes, returnType)
        ) {
          val candidatesForArgs =
            for (argIdx <- 0 until arity) yield {
              val c = costs(argIdx)
              state.getLevelOfCost(c)(argTypes(argIdx))
            }

          val isRecCall = compName == name
          cartesianProduct(candidatesForArgs).foreach { product =>

            val valueVector = TimeTools.runOnce {
              (0 until exampleCount).map(exId => {
                val arguments = product.map(_._1(exId))
                if (isRecCall && !argDecrease(arguments, exId))
                  ValueError
                else
                  impl.executeEfficient(arguments)
              })
            }

            if (!config.deleteAllErr || notAllErr(valueVector)) {
              val term = Component(compName, product.map(_._2))
              state.registerTermAtLevel(cost, returnType, term, valueVector)
            }
          }
        }
      }
    }

    state.openNextLevel()
    inputTypes.indices.foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      state.registerTermAtLevel(1, inputTypes(argId), v(inputNames(argId)), valueMap)
    })

    val goalVM = outputs.zipWithIndex.map(_.swap).toMap

    (1 to config.maxCost).foreach(level => {
      TimeTools.printTimeUsed(s"synthesize related components at level $level"){
        synthesizeAtLevel(level, synBoolAndReturnType = true)
      }

      TimeTools.printTimeUsed("Goal searching") {
        state.createLibrariesForThisLevel()
        val search = new BatchGoalSearchLoose(
          level,
          termOfCostAndVM = state.libraryOfCost,
          termsOfCost = state.termsOfCost,
          boolOfVM = state.boolLibrary
        )
        search.searchMin(config.searchSizeFactor * level, goalVM).foreach { case (c, term) =>
          return resultFromState(c, level, term)
        }
      }

      TimeTools.printTimeUsed(s"synthesize unrelated components"){
        synthesizeAtLevel(level, synBoolAndReturnType = false)
      }

      logger(s"State at level: $level\n")
      state.print(exampleCount)
      state.openNextLevel()
    })
    None
  }


}


