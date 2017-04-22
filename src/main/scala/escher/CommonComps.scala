package escher

import escher.Synthesis.ArgList

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

object ComponentImpl{
  def recursiveImpl(name: String, argNames: IS[String],
                    inputTypes: IS[Type], returnType: Type,
                    compMap: Map[String, ComponentImpl],
                    argListCompare: (ArgList,ArgList) => Boolean,
                    body: Term, debug: Boolean = false
                   ): ComponentImpl = {

    def impl(lastArg: Option[ArgList]): ComponentImpl = {
      ComponentImpl(inputTypes, returnType, {
        case args => lastArg match {
          case Some(la) if !argListCompare(args, la) =>
            ValueError
          case _ =>
            val newCompMap = compMap.updated(name, impl(Some(args)))
            val varMap = argNames.zip(args).toMap

            if (debug) {
              println(s"[Call $name]")
              val t = Term.executeTermDebug(
                varMap = varMap,
                compMap = newCompMap
              )(body)
              println(s"[$name Called]")
              t
            } else {
              Term.executeTerm(
                varMap = varMap,
                compMap = newCompMap
              )(body)
            }
        }
      })
    }
    impl(None)
  }
}

/** Commonly used components */
//noinspection TypeAnnotation
object CommonComps {

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
    "isNonNeg" -> isNonNeg,

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

  val standardComps = noTree ++ treeComps


  def createPair(t1: Type, t2: Type) = ComponentImpl(
    IS(t1, t2),
    tyPair(t1, t2),
    { case IS(v1, v2) => (v1, v2) }
  )

  val fst = ComponentImpl(
    IS(tyPair(tyVar(0), tyVar(1))),
    tyVar(0),
    { case IS(ValuePair(v)) => v._1 }
  )

  val snd = ComponentImpl(
    IS(tyPair(tyVar(0), tyVar(1))),
    tyVar(1),
    { case IS(ValuePair(v)) => v._2 }
  )

  def pairComps(t1: Type, t2: Type) = Map(
    "createPair" -> createPair(t1,t2),
    "fst" -> fst,
    "snd" -> snd
  )

  val insert = ComponentImpl(
    IS(tyList(tyVar(0)), tyInt, tyVar(0)), tyList(tyVar(0)),
    impl = {
      case IS(ValueList(xs), ValueInt(i), v) =>
        val (l,r) = xs.splitAt(i)
        l ++ (v::r)
    }
  )

  val reverse = ComponentImpl(
    IS(tyList(tyVar(0))), tyList(tyVar(0)),
    impl = {
      case IS(ValueList(xs)) => xs.reverse
    }
  )

  val stutter = {
    def stutterF[A](xs: List[A]): List[A] = {
      if(xs.isEmpty) List()
      else xs.head :: xs.head :: stutterF(xs.tail)
    }
    ComponentImpl(
      IS(tyList(tyVar(0))), tyList(tyVar(0)),
      impl = {
        case IS(ValueList(xs)) => stutterF(xs)
      }
    )
  }

  /** cartesian product of two lists */
  val cartesian = {
    def cardF[A,B](xs: List[A], ys: List[B]): List[(A,B)] = {
      xs.flatMap(x => ys.map(y => (x,y)))
    }
    ComponentImpl(
      IS(tyList(tyVar(0)), tyList(tyVar(1))), tyList(tyPair(tyVar(0), tyVar(1))),
      impl = {
        case IS(ValueList(xs), ValueList(ys)) =>
          ValueList(cardF(xs,ys).map(ValuePair))
      }
    )
  }

  val squareList = ComponentImpl(
    IS(tyInt), tyList(tyInt),
    impl = {
      case IS(ValueInt(n)) =>
        (0 to n).map(x => x* x).toList
    }
  )

  val times = ComponentImpl(IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => ValueInt(x * y)}
  )

  val div = ComponentImpl(IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => if(y == 0) ValueError else ValueInt(x / y)}
  )

  val timesAndDiv = Map(
    "times" -> CommonComps.times,
    "div" -> CommonComps.div
  )

  /** 1,1,2,3,5,8,... */
  val fib = {
    def fibF(n: Int): Int = {
      var a,b = 1
      (0 until n).foreach{_ =>
        val c = a + b
        a = b
        b = c
      }
      a
    }

    ComponentImpl(IS(tyInt), tyInt,
      impl = { case IS(ValueInt(n)) => fibF(n)}
    )
  }

  val modulo = {
    ComponentImpl(IS(tyInt, tyInt), tyInt, impl = {
      case IS(ValueInt(a), ValueInt(b)) =>
        if(b == 0) ValueError else a % b
    })
  }

  val gcd = {
    def gcdF(a: Int, b: Int): Int = {
      if(b == 0) a
      else gcdF(b, a % b)
    }

    ComponentImpl(IS(tyInt, tyInt), tyInt,
      impl = { case IS(ValueInt(a), ValueInt(b)) => gcdF(a,b)
      }
    )
  }

  val compress = {
    def f[A](xs: List[A]): List[A] = xs match {
      case a::b::tail => if(a==b) f(b::tail) else a :: f(b::tail)
      case _ => xs
    }

    ComponentImpl(IS(tyList(tyVar(0))), tyList(tyVar(0)),
      impl = { case IS(ValueList(xs)) => f(xs) }
    )
  }

  val nodesAtLevel = {
    def impl[A](tree: BinaryTree[A], level: Int): List[A] = tree match {
      case BinaryNode(tag, left, right) =>
        if(level == 0) List(tag)
        else if(level > 0) impl(left, level-1) ++ impl(right, level-1)
        else List()
      case BinaryLeaf => List()
    }

    ComponentImpl(IS(tyTree(tyVar(0)), tyInt), tyTree(tyVar(0)),
      impl = { case IS(ValueTree(tree), ValueInt(level)) =>
        impl(tree, level)
      }
    )
  }
}
