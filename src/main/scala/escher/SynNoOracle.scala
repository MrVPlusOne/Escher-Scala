package escher

import SynNoOracle._
import escher.Term.Component
import Synthesis._
import escher.CommonComps.ReducibleCheck



class SynNoOracle(config: Config, logger: String => Unit){

  def synthesize(name: String, inputTypesFree: IndexedSeq[Type], inputNames: IndexedSeq[String], returnTypeFree: Type)
                (envComps: Set[ComponentImpl],
                 examples0: IS[(ArgList, TermValue)],
                 compReductionRules: Map[ComponentImpl, ReducibleCheck]): Option[SynthesizedComponent] = {
    import DSL._

    val examples = examples0.sortWith(Synthesis.exampleLt)
    val inputs: IS[ArgList] = examples.map(_._1)
    val outputs: IS[TermValue] = examples.map(_._2)
    val inputTypes = inputTypesFree.map(_.fixVars)
    val goalReturnType = returnTypeFree.fixVars
    val exampleCount = outputs.length
    val state = new SynthesisState(examples, TypeMap.empty(), goalReturnType, compReductionRules)
    val signature = ComponentSignature(name, inputNames, inputTypes, goalReturnType)

    require(inputTypes.length == inputNames.length)

    def argDecrease(arg: ArgList, exampleId: Int) = {
      config.argListCompare(arg, inputs(exampleId))
    }

    val knownMap = examples.toMap
    val recursiveComp = ExtendedCompImpl.fromImplOnTermValue(name, inputTypes, goalReturnType, impl = {
      argList => knownMap.getOrElse(argList, ValueUnknown)
    })

    val envExtendedCompSet = envComps.map{ impl =>
      ExtendedCompImpl.fromImplOnTermValue(impl.name, impl.inputTypes, impl.returnType, impl.executeEfficient)
    }
    val compSet: Set[ExtendedCompImpl] = envExtendedCompSet ++ Set(recursiveComp)

    def compCostFunction(impl: ExtendedCompImpl): Int = {
      1
    }

    val isInterestingSignature = Synthesis.isInterestingSignature(goalReturnType, inputTypes)

    def synthesizeAtLevel(cost: Int, synBoolAndReturnType: Boolean): Unit = {
      for (
        impl <- compSet;
        isRecCall = impl.name == name;
        compCost = compCostFunction(impl) if compCost <= cost
      ) {
        val arity = impl.inputTypes.length
        val costLeft = cost - compCost
        if (arity == 0) {
          if (compCost == cost) {
            val result = impl.execute(IS()).asInstanceOf[TermValue]
            val valueVector = (0 until exampleCount).map(_ => result)
            val term = Component(impl.name, IS())
            state.registerNonRecAtLevel(cost, impl.returnType, term, valueVector)
          }
        } else {
          for (
            costs <- divideNumberAsSum(costLeft, arity, minNumber = 1);
            (argTypes, returnType) <- typesForCosts(c => state.getNonRecOfCost(c).typesIterator, costs, impl.inputTypes, impl.returnType)
            if (synBoolAndReturnType == (goalReturnType.instanceOf(returnType) || tyBool.instanceOf(returnType))) &&
              isInterestingSignature(argTypes, returnType)
          ) {

            // non-recursive terms
            val nonRecCandidates =
              for (argIdx <- 0 until arity) yield {
                val c = costs(argIdx)
                state.getNonRecOfCost(c)(argTypes(argIdx)).toIndexedSeq
              }

            if (isRecCall) {
              cartesianProduct(nonRecCandidates).foreach { product =>
                val valueVector =
                  (0 until exampleCount).map(exId => {
                    val arguments = product.map(_._1(exId))
                    if (!argDecrease(arguments, exId))
                      ValueError
                    else
                      impl.execute(arguments)
                  })

                if (!config.deleteAllErr || notAllErr(valueVector)) {
                  val term = Component(impl.name, product.map(_._2))
                  state.registerTermAtLevel(cost, returnType, term, valueVector)
                }
              }
            } else {
              // recursive terms generation
              val recCandidates =
                for (argIdx <- 0 until arity) yield {
                  val c = costs(argIdx)
                  state.getRecOfCost(c)(argTypes(argIdx)).toIndexedSeq.map(_.swap)
                }
              val allCandidates = (nonRecCandidates zip recCandidates).map(x => x._1 ++ x._2)

              cartesianProduct(allCandidates).foreach { product =>
                val valueVector =
                  (0 until exampleCount).map(exId => {
                    val arguments = product.map(_._1(exId))
                    impl.execute(arguments)
                  })

                if (!config.deleteAllErr || notAllErr(valueVector)) {
                  val term = Component(impl.name, product.map(_._2))
                  state.registerTermAtLevel(cost, returnType, term, valueVector)
                }
              }
            }

          }
        }
      }
    }

    def resultFromState(cost: Int, level: Int, term: Term): Option[SynthesizedComponent] = {
      Some(SynthesizedComponent(signature, term, cost, level))
    }


    state.openNextLevel()
    inputTypes.indices.foreach(argId =>{
      val valueMap = inputs.indices.map(exId => {
        inputs(exId)(argId)
      })
      state.registerNonRecAtLevel(1, inputTypes(argId), v(inputNames(argId)), valueMap)
    })

    val goalVM = outputs.zipWithIndex.map(_.swap).toMap

    (1 to config.maxCost).foreach(level => {
      TimeTools.printTimeUsed(s"synthesize related components at level $level"){
        synthesizeAtLevel(level, synBoolAndReturnType = true)
      }

      logger(s"State at level: $level\n")
      state.print(exampleCount)

      if(!config.onlyForwardSearch) {
        TimeTools.printTimeUsed("Goal searching") {
          state.createLibrariesForThisLevel()

          val search = new DynamicGoalSearch(
            level,
            signature,
            envComps,
            config.argListCompare,
            inputVector = inputs,
            termOfCostAndVM = state.libraryOfCost,
            termsOfCost = state.termsOfCost,
            boolOfVM = state.boolLibrary
          )
          search.searchMin(config.searchSizeFactor * level, goalVM,
            state.recTermsOfReturnType, fillTermToHole = { t => t },
            isFirstBranch = true,
            prefixTrigger = None
          ).foreach { case (c, term) =>
            return resultFromState(c, level, term)
          }
        }
      }

      TimeTools.printTimeUsed(s"synthesize unrelated components"){
        synthesizeAtLevel(level, synBoolAndReturnType = false)
      }

      state.openNextLevel()
    })
    None

  }

  import collection.mutable

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

  class RecTypeMap private(private val map: mutable.Map[Type, RecMap]){

    def apply(ty: Type): RecMap = {
      val v = map.getOrElse(ty, Map())
      map(ty) = v
      v
    }

    def update(ty: Type, term: Term, valueVec: ExtendedValueVec): Unit ={
      map(ty) = apply(ty).updated(term, valueVec)
    }

    def get(ty: Type): Option[RecMap] = map.get(ty)


    def print(indentation: Int): Unit = {
      val whiteSpace = " " * indentation
      map.mapValues{map =>
        val compList = map.map{case (term, vMap) => s"'${term.show}': ${ExtendedValueVec.show(vMap)}"}
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

  object RecTypeMap{
    def empty() = new RecTypeMap(mutable.Map())
  }


  class TypeMap private(private val map: mutable.Map[Type, ValueTermMap]){

    def apply(ty: Type): ValueTermMap = {
      val v = map.getOrElse(ty, ValueTermMap.empty())
      map(ty) = v
      v
    }

    def get(ty: Type): Option[ValueTermMap] = map.get(ty)


    def print(indentation: Int): Unit = {
      val whiteSpace = " " * indentation
      map.mapValues{map =>
        val compList = map.map{case (vMap, term) => {
          if(vMap.contains(None)){
            println("WTF??")
          }
          s"'${term.show}': ${ValueVector.show(vMap)}"
        }}
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

  class SynthesisState(val examples: IS[(ArgList,TermValue)], val totalNonRec: TypeMap, returnType: Type,
                       reductionRules: Map[ComponentImpl, ReducibleCheck]) {
    import DSL._

    val reductionRulesMap: Map[String, ReducibleCheck] = reductionRules.map{
      case (impl, rule) => impl.name -> rule
    }

    private var _levelNonRecs: IS[TypeMap] = IS()
    private var _levelRecComps: IS[RecTypeMap] = IS()

    def levels: Int = _levelNonRecs.length

    def getNonRecOfCost(cost: Int): TypeMap = _levelNonRecs(cost-1)

    def getRecOfCost(cost: Int): RecTypeMap = _levelRecComps(cost - 1)

    def openNextLevel(): Int = {
      _levelNonRecs = _levelNonRecs :+ TypeMap.empty()
      _levelRecComps = _levelRecComps :+ RecTypeMap.empty()
      _levelNonRecs.length
    }

    private var returnTypeVectorTrees: IS[ValueVectorTree[Term]] = IS()
    private var boolVectorTrees: IS[ValueVectorTree[Term]] = IS()
    var recTermsOfReturnType: IS[Seq[(Term, ExtendedValueVec)]] = IS()

    def createLibrariesForThisLevel(): Unit ={
      require(returnTypeVectorTrees.length == levels - 1)
      require(boolVectorTrees.length == levels - 1)

      val returnTypeTree = new ValueVectorTree[Term](examples.length)
      val typeMap = getNonRecOfCost(levels)
      for(ty <- typesMatch(typeMap.typesIterator, returnType)){
        val vt = typeMap(ty)
        vt.foreach{ case (vv, term) => returnTypeTree.addTerm(term, vv) }
      }
      returnTypeVectorTrees = returnTypeVectorTrees :+ returnTypeTree

      val boolTree = new ValueVectorTree[Term](examples.length)
      for(ty <- typesMatch(typeMap.typesIterator, tyBool)){
        val vt = typeMap(ty)
        vt.foreach{ case (vv, term) => boolTree.addTerm(term, vv) }
      }
      boolVectorTrees = boolVectorTrees :+ boolTree

      var newRecs: IS[(Term, ExtendedValueVec)] = IS()
      val recTypeMap = getRecOfCost(levels)
      for(ty <- typesMatch(recTypeMap.typesIterator, returnType)){
        val recMap = recTypeMap(ty)
        newRecs = newRecs ++ recMap
      }
      recTermsOfReturnType = recTermsOfReturnType :+ newRecs
    }

    private def typesMatch(types: Iterator[Type], ty: Type): List[Type] = {
      types.collect{
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
      (0 to n - _levelNonRecs.length).foreach(_ => openNextLevel())
    }


    def registerNonRecAtLevel(cost: Int, ty: Type, term: Term, valueVector: ValueVector): Boolean = {
      val ty1 = Type.alphaNormalForm(ty)

      if(totalNonRec(ty1).get(valueVector).nonEmpty)
        return false

      totalNonRec(ty1)(valueVector) = term
      getNonRecOfCost(cost)(ty1)(valueVector) = term
      false
    }

    def registerRecTermAtLevel(cost: Int, ty: Type, term: Term, valueVector: ExtendedValueVec): Boolean = {
      if(config.useReductionRules) {
        term match {
          case Component(n, terms) if reductionRulesMap contains n =>
            if (reductionRulesMap(n).isReducible(terms))
              return false
          case _ =>
        }
      }
      val ty1 = Type.alphaNormalForm(ty)
      getRecOfCost(cost)(ty1, term) = valueVector
      true
    }

    def registerTermAtLevel(cost: Int, ty: Type, term: Term, valueVector: ExtendedValueVec): Boolean = {
      ExtendedValueVec.toValueVec(valueVector) match {
        case Some(x) => registerNonRecAtLevel(cost, ty, term, x)
        case None => registerRecTermAtLevel(cost, ty, term, valueVector)
      }
    }


    def print(exampleCount: Int): Unit = {
      logLn(config.logTotalMap)(s"TotalMap: (${totalNonRec.statString})")
      if(config.logTotalMap && config.logComponents){
        totalNonRec.print(4)
      }
      logLn(config.logLevels)(s"Non-recursive LevelMaps:")
      if(config.logLevels)
        _levelNonRecs.indices.lastOption.foreach{i =>
          val c = i + 1
          val typeMap = getNonRecOfCost(c)
          logLn(condition = true)(s"  $c: (${typeMap.statString})")
          if(config.logComponents)
            typeMap.print(6)
        }
      logLn(config.logLevels)(s"Recursive LevelMaps:")
      if(config.logLevels)
        _levelRecComps.indices.lastOption.foreach{i =>
          val c = i + 1
          val recTypeMap = getRecOfCost(c)
          logLn(condition = true)(s"  $c: (${recTypeMap.statString})")
          if(config.logComponents)
            recTypeMap.print(6)
        }
    }
  }

}


object SynNoOracle {
  type RecMap = Map[Term, ExtendedValueVec]

  case class Config(
                     maxCost: Int = Int.MaxValue,
                     deleteAllErr: Boolean = true,
                     logGoal: Boolean = true,
                     logLevels: Boolean = true,
                     logComponents: Boolean = true,
                     logTotalMap: Boolean = true,
                     logReboot: Boolean = true,
                     argListCompare: (ArgList, ArgList) => Boolean = ArgList.anyArgSmaller,
                     searchSizeFactor: Int = 3,
                     useReductionRules: Boolean = true,
                     onlyForwardSearch: Boolean = false
                   )

  case class ExtendedCompImpl(name: String, inputTypes: IS[Type], returnType: Type,
                              execute: IS[ExtendedValue] => ExtendedValue)

  object ExtendedCompImpl{
    def fromImplOnTermValue(name: String, inputTypes: IS[Type], returnType: Type, impl: ArgList => ExtendedValue): ExtendedCompImpl = {
      def execute(args: IS[ExtendedValue]): ExtendedValue = {
        ExtendedValueVec.toValueVec(args) match {
          case None =>
            if(args.contains(ValueError)) ValueError else ValueUnknown
          case Some(knownArgs) => impl(knownArgs)
        }
      }
      ExtendedCompImpl(name, inputTypes, returnType, execute)
    }
  }



}