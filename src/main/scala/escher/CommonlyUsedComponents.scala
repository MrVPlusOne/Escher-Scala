package escher

/** An Exception which will not be caught by ComponentImpl.execute */
case class ExecutionError(msg: String) extends Exception {
  override def getMessage: String = msg

  override def getLocalizedMessage: String = msg
}

case class ComponentImpl(inputTypes: IS[Type], returnType: Type,
                         impl: PartialFunction[IS[TermValue],TermValue]){

  def shiftTypeId(amount: Int) = ComponentImpl(inputTypes.map(_.shiftId(amount)), returnType.shiftId(amount), impl)

  def execute(args: IS[TermValue], debug: Boolean): TermValue = {
    if(args.contains(ValueError))
      return ValueError

    val r = try {
      impl.apply(args)
    }
    catch {
      case _: MatchError =>
        if(debug) {
          println(s"[Warning] Match error in execute!")
          println(s"  input types: $inputTypes")
          println(s"  args: $args")
        }
        ValueError
      case e: ExecutionError => throw e
      case e: Exception =>
        if(debug){
          println(e)
        }
        ValueError
    }
    if(debug && r.matchType(returnType, returnType.nextFreeId).isEmpty) {
      throw ExecutionError(s"[Warning] Component return value type not match!\n"+
        s"  output type: $returnType\n  return value: $r"
      )
    }
    r
  }
}

object CommonlyUsedComponents {
  import DSL._

  val length = ComponentImpl(IS(TList of tyVar(0)), tyInt,
    impl = { case IS(ValueList(elems)) => ValueInt(elems.length) }
  )

  val isEmpty = ComponentImpl(IS(TList of tyVar(0)), tyBool,
    impl = { case IS(ValueList(elems)) => ValueBool(elems.isEmpty)}
  )

  val isZero = ComponentImpl(IS(tyInt), tyBool,
    impl = { case IS(ValueInt(x)) => x == 0 }
  )

  val isNonNeg = ComponentImpl(IS(tyInt), tyBool,
    impl = { case IS(ValueInt(x)) => x >= 0 }
  )

  val zero = ComponentImpl(
    inputTypes = IS(),
    returnType = tyInt,
    impl = { case IS() => ValueInt(0)}
  )

  val inc = ComponentImpl(IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(x + 1)}
  )

  val dec = ComponentImpl(IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(x - 1)}
  )

  val neg = ComponentImpl(IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(-x)}
  )

  val div2 = ComponentImpl(IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(x/2)}
  )

  val tail = ComponentImpl(IS(TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(ValueList(elems)) => ValueList(elems.tail)}
  )

  val cons = ComponentImpl(IS(tyVar(0), TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(x, ValueList(x2)) => ValueList(x :: x2)}
  )

  val head = ComponentImpl(IS(TList of tyVar(0)), tyVar(0),
    impl = { case IS(ValueList(xs)) => xs.head }
  )

  val emptyList = ComponentImpl(IS(), tyList(tyVar(0)),
    impl = { case IS() => listValue() }
  )

  val concat = ComponentImpl(IS(TList of tyVar(0), tyList(tyVar(0))), TList of tyVar(0),
    impl = { case IS(ValueList(xs), ValueList(ys)) => ValueList(xs ++ ys)}
  )

  val equal = ComponentImpl(IS(tyVar(0), tyVar(0)), tyBool,
    impl = { case IS(a,b) => ValueBool(a == b) }
  )

  val or = ComponentImpl(IS(tyBool, tyBool), tyBool,
    impl = { case IS(ValueBool(a),ValueBool(b)) => a || b }
  )

  val and = ComponentImpl(IS(tyBool, tyBool), tyBool,
    impl = { case IS(ValueBool(a),ValueBool(b)) => a && b }
  )

  val not = ComponentImpl(IS(tyBool), tyBool,
    impl = { case IS(ValueBool(a)) => !a }
  )

  val plus = ComponentImpl(IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => ValueInt(x + y)}
  )

  val noTree = Map(
    // boolean
    "and" -> and,
    "or" -> or,
    "not" -> not,
    "equal" -> equal,
    "isEmpty" -> isEmpty,
    "isNN" -> isNonNeg,

    // list
    "head" -> head,
    "tail" -> tail,
    "cons" -> cons,
    "concat" -> concat,
    "emptyList" -> emptyList,

    // integer
    "zero" -> zero,
    "inc" -> inc,
    "dec" -> dec,
    "neg" -> neg,
    "length" -> length,
    "plus" -> plus,
    "div2" -> div2

  )

  val createLeaf = ComponentImpl(IS(), tyTree(tyVar(0)),
    impl = { case IS() => ValueTree(BinaryLeaf) }
  )

  val createNode = ComponentImpl(
    IS(tyVar(0), tyTree(tyVar(0)), tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(v, ValueTree(l), ValueTree(r)) => ValueTree(BinaryNode(v, l, r)) }
  )

  val isLeaf = ComponentImpl(
    IS(tyTree(tyVar(0))),
    tyBool,
    impl = { case IS(ValueTree(t)) => t == BinaryLeaf}
  )

  val treeValue = ComponentImpl(
    IS(tyTree(tyVar(0))),
    tyVar(0),
    impl = { case IS(ValueTree(t)) => t.asInstanceOf[BinaryNode[TermValue]].tag }
  )

  val treeLeft = ComponentImpl(
    IS(tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(ValueTree(t)) => t.asInstanceOf[BinaryNode[TermValue]].left }
  )

  val treeRight = ComponentImpl(
    IS(tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(ValueTree(t)) => t.asInstanceOf[BinaryNode[TermValue]].right }
  )

  val treeComps = Map(
    "createLeaf" -> createLeaf,
    "createNode" -> createNode,
    "isLeaf" -> isLeaf,
    "treeValue" -> treeValue,
    "treeLeft" -> treeLeft,
    "treeRight" -> treeRight
  )


  def recursiveImpl(name: String, argNames: IS[String],
                  inputTypes: IS[Type], outputType: Type,
                  compMap: Map[String, ComponentImpl],
                  body: Term, debug: Boolean = false
                 ): ComponentImpl = {

    lazy val impl: ComponentImpl = {
      lazy val newCompMap = compMap.updated(name, impl)
      if(debug)
        ComponentImpl(inputTypes, outputType,
          { case args =>
            println(s"[Call $name]")
            val varMap = argNames.zip(args).toMap
            val t = Term.executeTermDebug(
              varMap = varMap,
              compMap = newCompMap
            )(body)
            println(s"[$name Called]")
            t
          }
        )
      else
        ComponentImpl(inputTypes, outputType,
          { case args =>
            val varMap = argNames.zip(args).toMap
            val t = Term.executeTerm(
              varMap = varMap,
              compMap = newCompMap
            )(body)
            t
          }
        )
    }
    impl
  }


}
