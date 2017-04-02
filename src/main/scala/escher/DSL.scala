package escher

/**
  * using "import DSL._" to write terms more easily
  */
object DSL {
  import Term._
  import Type._


  implicit class ComponentFromString(name: String){
    /** allowing us to write <i>"Component(n, args*)"</i> as <i>"n $ (args*)"</i> */
    def $ (args: Term*) = Component(name, args.toList)
  }

  def v(name: String) = Var(name)

  def c(name: String, args: Term*) = Component(name, args.toList)

  def `if`(condition: Term)(thenBranch: Term)(elseBranch: Term) = If(condition, thenBranch, elseBranch)

  def `var`(name: String) = Var(name)

  def tVar(id: Int) = TVar(id)

  implicit def intConversion(i: Int): TermInt = TermInt(i)
  implicit def boolConversion(b: Boolean): TermBool = TermBool(b)

  def listValue(terms: TermValue*) = TermList(terms.toList)

}
