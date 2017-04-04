package escher

/**
  * using "import DSL._" to write terms more easily
  */
object DSL {
  import Term._
  import Type._


  implicit class ComponentFromString(name: String){
    /** allowing us to write <i>"Component(n, args*)"</i> as <i>"n $ (args*)"</i> */
    def $ (args: Term*) = Component(name, args.toIndexedSeq)
  }

  def v(name: String) = Var(name)

  def c(name: String, args: Term*) = Component(name, args.toIndexedSeq)

  def `if`(condition: Term)(thenBranch: Term)(elseBranch: Term) = If(condition, thenBranch, elseBranch)

  def `var`(name: String) = Var(name)

  def tyVar(id: Int) = TVar(id)

  def tyFixVar(id: Int) = TFixedVar(id)

  implicit def intConversion(i: Int): ValueInt = ValueInt(i)
  implicit def boolConversion(b: Boolean): ValueBool = ValueBool(b)

  def listValue(terms: TermValue*) = ValueList(terms.toList)

  val tyInt: TApply = TInt.of()
  val tyBool: TApply = TBool.of()
  def tyList(params: Type*): TApply = TList.of(params:_*)

}
