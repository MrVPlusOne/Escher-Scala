package escher

import Term._

case class ComponentImpl(inputTypes: List[Type], outputType: Type,
                         impl: PartialFunction[List[TermValue],TermValue]){
  def execute(args: List[TermValue]): TermValue = {
    try { impl.apply(args) }
    catch {
      case _: MatchError =>
        println(s"[Warning] Match error in execute!")
        println(s"  input types: $inputTypes")
        println(s"  args: $args")
        TermError
      case _: Exception => TermError
    }
  }
}

object CommonlyUsedComponents {
  import DSL._

  val length = ComponentImpl(List(TList of tVar(0)), TInt.of(),
    impl = { case List(TermList(elems)) => TermInt(elems.length) }
  )

  val isEmpty = ComponentImpl(List(TList of tVar(0)), TBool.of(),
    impl = { case List(TermList(elems)) => TermBool(elems.isEmpty)}
  )

  val zero = ComponentImpl(
    inputTypes = List(),
    outputType = TInt.of(),
    impl = { case List() => TermInt(0)}
  )

  val inc = ComponentImpl(List(TInt of ()), TInt of (),
    impl = { case List(TermInt(x)) => TermInt(x + 1)}
  )

  val tail = ComponentImpl(List(TList of tVar(0)), TList of tVar(0),
    impl = { case List(TermList(elems)) => TermList(elems.tail)}
  )

  val cons = ComponentImpl(List(tVar(0), TList of tVar(0)), TList of tVar(0),
    impl = { case List(x, TermList(x2)) => TermList(x :: x2)}
  )

  val allMap = Map(
    "length" -> length,
    "isEmpty" -> isEmpty,
    "zero" -> zero,
    "inc" -> inc,
    "tail" -> tail,
    "cons" -> cons
  )

  def recursiveImpl(name: String, argNames: List[String],
                  inputTypes: List[Type], outputType: Type,
                  compMap: String => ComponentImpl,
                  body: Term, debug: Boolean = false
                 ): ComponentImpl = {

    lazy val impl: ComponentImpl = {
      lazy val newCompMap = (s: String) => if(s == name) impl else compMap(s)
      if(debug)
        ComponentImpl(inputTypes, outputType,
          { case args =>
            println(s"[Call $name]")
            val varMap = argNames.zip(args).toMap
            val t = Term.executeTermDebug(
              varMap = varMap.apply,
              compMap = newCompMap.apply
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
              varMap = varMap.apply,
              compMap = newCompMap.apply
            )(body)
            t
          }
        )
    }
    impl
  }


}
