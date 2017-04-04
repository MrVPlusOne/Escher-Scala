package escher

/** An Exception which will not be caught by ComponentImpl.execute */
case class ExecutionError(msg: String) extends Exception {
  override def getMessage: String = msg

  override def getLocalizedMessage: String = msg
}

case class ComponentImpl(inputTypes: List[Type], outputType: Type,
                         impl: PartialFunction[List[TermValue],TermValue]){
  def execute(args: List[TermValue], debug: Boolean): TermValue = {
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
    if(debug && r.matchType(outputType, outputType.nextFreeId).isEmpty) {
      throw ExecutionError(s"[Warning] Component return value type not match!\n"+
        s"  output type: $outputType\n  return value: $r"
      )
    }
    r
  }
}

object CommonlyUsedComponents {
  import DSL._

  val length = ComponentImpl(List(TList of tyVar(0)), TInt.of(),
    impl = { case List(ValueList(elems)) => ValueInt(elems.length) }
  )

  val isEmpty = ComponentImpl(List(TList of tyVar(0)), TBool.of(),
    impl = { case List(ValueList(elems)) => ValueBool(elems.isEmpty)}
  )

  val isZero = ComponentImpl(List(TInt.of()), TBool.of(),
    impl = { case List(ValueInt(x)) => x == 0 }
  )

  val zero = ComponentImpl(
    inputTypes = List(),
    outputType = TInt.of(),
    impl = { case List() => ValueInt(0)}
  )

  val inc = ComponentImpl(List(TInt of ()), TInt of (),
    impl = { case List(ValueInt(x)) => ValueInt(x + 1)}
  )

  val dec = ComponentImpl(List(TInt of ()), TInt of (),
    impl = { case List(ValueInt(x)) => ValueInt(x - 1)}
  )

  val tail = ComponentImpl(List(TList of tyVar(0)), TList of tyVar(0),
    impl = { case List(ValueList(elems)) => ValueList(elems.tail)}
  )

  val cons = ComponentImpl(List(tyVar(0), TList of tyVar(0)), TList of tyVar(0),
    impl = { case List(x, ValueList(x2)) => ValueList(x :: x2)}
  )

  val equal = ComponentImpl(List(tyVar(0), tyVar(0)), TBool.of(),
    impl = { case List(a,b) => ValueBool(a == b) }
  )

  val or = ComponentImpl(List(TBool.of(), TBool.of()), TBool.of(),
    impl = { case List(ValueBool(a),ValueBool(b)) => a || b }
  )

  val plus = ComponentImpl(List(TInt.of(), TInt.of()), TInt.of(),
    impl = { case List(ValueInt(x), ValueInt(y)) => ValueInt(x + y)}
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

  def recursiveImpl(name: String, argNames: List[String],
                  inputTypes: List[Type], outputType: Type,
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
