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

  val length = ComponentImpl(IS(TList of tyVar(0)), TInt.of(),
    impl = { case IS(ValueList(elems)) => ValueInt(elems.length) }
  )

  val isEmpty = ComponentImpl(IS(TList of tyVar(0)), TBool.of(),
    impl = { case IS(ValueList(elems)) => ValueBool(elems.isEmpty)}
  )

  val isZero = ComponentImpl(IS(TInt.of()), TBool.of(),
    impl = { case IS(ValueInt(x)) => x == 0 }
  )

  val zero = ComponentImpl(
    inputTypes = IS(),
    returnType = TInt.of(),
    impl = { case IS() => ValueInt(0)}
  )

  val inc = ComponentImpl(IS(TInt of ()), TInt of (),
    impl = { case IS(ValueInt(x)) => ValueInt(x + 1)}
  )

  val dec = ComponentImpl(IS(TInt of ()), TInt of (),
    impl = { case IS(ValueInt(x)) => ValueInt(x - 1)}
  )

  val tail = ComponentImpl(IS(TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(ValueList(elems)) => ValueList(elems.tail)}
  )

  val cons = ComponentImpl(IS(tyVar(0), TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(x, ValueList(x2)) => ValueList(x :: x2)}
  )

  val equal = ComponentImpl(IS(tyVar(0), tyVar(0)), TBool.of(),
    impl = { case IS(a,b) => ValueBool(a == b) }
  )

  val or = ComponentImpl(IS(TBool.of(), TBool.of()), TBool.of(),
    impl = { case IS(ValueBool(a),ValueBool(b)) => a || b }
  )

  val plus = ComponentImpl(IS(TInt.of(), TInt.of()), TInt.of(),
    impl = { case IS(ValueInt(x), ValueInt(y)) => ValueInt(x + y)}
  )

  val allMap = Map(
    "tail" -> tail,
    "cons" -> cons,

    "zero" -> zero,
    "inc" -> inc,
    "dec" -> dec,
    "length" -> length,
    "plus" -> plus,

    "isEmpty" -> isEmpty,
    "isZero" -> isZero,
    "or" -> or,
    "equal" -> equal

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
