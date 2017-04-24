package escher

import escher.CommonComps.ReducibleCheck
import escher.Synthesis.ArgList

/** An Exception which will not be caught by ComponentImpl.execute */
case class ExecutionError(msg: String) extends Exception {
  override def getMessage: String = msg

  override def getLocalizedMessage: String = msg
}


case class ComponentImpl(name: String, inputTypes: IS[Type], returnType: Type,
                         impl: PartialFunction[IS[TermValue],TermValue],
                         callByValue: Boolean = true){

  def shiftTypeId(amount: Int) = ComponentImpl(name, inputTypes.map(_.shiftId(amount)), returnType.shiftId(amount), impl)

  def execute(args: IS[TermValue], debug: Boolean): TermValue = {
    if(callByValue && args.contains(ValueError))
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

  def executeEfficient(args: IS[TermValue]): TermValue = {
    if(callByValue && args.contains(ValueError))
      return ValueError
    impl.applyOrElse(args, (_: IS[TermValue]) => ValueError)
  }
}

object ComponentImpl{
  def recursiveImpl(name: String, argNames: IS[String],
                    inputTypes: IS[Type], returnType: Type,
                    envComps: Set[ComponentImpl],
                    argListCompare: (ArgList,ArgList) => Boolean,
                    body: Term, debug: Boolean = false
                   ): ComponentImpl = {
    val compMap = envComps.map(x => x.name -> x).toMap

    import collection.mutable

    def impl(lastArg: Option[ArgList], buffer: mutable.Map[ArgList, TermValue]): ComponentImpl = {
      ComponentImpl(name, inputTypes, returnType, {
        case args => lastArg match {
          case Some(la) if !argListCompare(args, la) =>
            ValueError
          case _ =>
            buffer.get(args) match{
              case Some(v) => v
              case None =>
                val newCompMap = compMap.updated(name, impl(Some(args), buffer))
                val varMap = argNames.zip(args).toMap

                val result = if (debug) {
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
                buffer(args) = result
                result
            }
        }
      })
    }
    impl(None, mutable.Map())
  }
}

/** Commonly used components */
//noinspection TypeAnnotation
object CommonComps {

  case class ReducibleCheck(arity: Int, f: PartialFunction[IS[Term], Boolean]){
    def isReducible(args: IS[Term]): Boolean = {
      require(args.length == arity)
      f.applyOrElse(args, (_: IS[Term]) => false)
    }
  }
  type CompMap = Map[String, (ComponentImpl, ReducibleCheck)]

  object ReducibleCheck{
    def reduces(rules: ReducibleCheck*): ReducibleCheck = {
      require(rules.nonEmpty)
      val arity = rules.head.arity
      require(rules.forall(_.arity == arity))

      def f(args: IS[Term]): Boolean = {
        for (r <- rules) {
          if (r.isReducible(args)) return true
        }
        false
      }
      ReducibleCheck(arity, PartialFunction(f))
    }

    val commutative: ReducibleCheck = ReducibleCheck(2, {
      case IS(t1,t2) => t1 > t2
    })

    def noDirectChild(child: ComponentImpl): ReducibleCheck = ReducibleCheck(1, {
      case IS(Term.Component(n, _)) => n == child.name
    })

    val argsDifferent = ReducibleCheck(2, {
      case IS(t1, t2) => t1 == t2
    })

    def associative(op: ComponentImpl) = ReducibleCheck(2, {
      case IS(_, Term.Component(n, _)) => n == op.name
    })
  }
  import ReducibleCheck._

  import DSL._

  val length = ComponentImpl("length", IS(TList of tyVar(0)), tyInt,
    impl = { case IS(ValueList(elems)) => ValueInt(elems.length) }
  )

  val isNil = ComponentImpl("isNil", IS(TList of tyVar(0)), tyBool,
    impl = { case IS(ValueList(elems)) => ValueBool(elems.isEmpty)}
  )

  val isZero = ComponentImpl("isZero", IS(tyInt), tyBool,
    impl = { case IS(ValueInt(x)) => x == 0 }
  )

  val isNonNeg = ComponentImpl("isNonNeg", IS(tyInt), tyBool,
    impl = { case IS(ValueInt(x)) => x >= 0 }
  )

  val zero = ComponentImpl(
    "zero",
    inputTypes = IS(),
    returnType = tyInt,
    impl = { case IS() => ValueInt(0)}
  )

  val inc = ComponentImpl("inc", IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(x + 1)}
  )

  val dec = ComponentImpl("dec", IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(x - 1)}
  )

  val neg = ComponentImpl("neg", IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(-x)}
  )

  val div2 = ComponentImpl("div2", IS(tyInt), tyInt,
    impl = { case IS(ValueInt(x)) => ValueInt(x/2)}
  )

  val tail = ComponentImpl("tail", IS(TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(ValueList(_::xs)) => ValueList(xs)}
  )

  val cons = ComponentImpl("cons", IS(tyVar(0), TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(x, ValueList(x2)) => ValueList(x :: x2)}
  )

  val head = ComponentImpl("head", IS(TList of tyVar(0)), tyVar(0),
    impl = { case IS(ValueList(x::_)) => x }
  )

  val nil = ComponentImpl("nil", IS(), tyList(tyVar(0)),
    impl = { case IS() => listValue() }
  )

  val concat = ComponentImpl("concat", IS(TList of tyVar(0), tyList(tyVar(0))), TList of tyVar(0),
    impl = { case IS(ValueList(xs), ValueList(ys)) => ValueList(xs ++ ys)}
  )

  val T = ComponentImpl("T", IS(), tyBool,
    impl = { case IS() => true }
  )

  val F = ComponentImpl("F", IS(), tyBool,
    impl = { case IS() => false }
  )

  val equal = ComponentImpl("equal", IS(tyVar(0), tyVar(0)), tyBool,
    impl = { case IS(a,b) => ValueBool(a == b) }
  )

  def or_impl(callByValue: Boolean) = ComponentImpl(
    "or",
    IS(tyBool, tyBool), tyBool,
    impl = {
      case IS(ValueBool(true), _) => true
      case IS(ValueBool(false), ValueBool(b)) => b
      case _ => ValueError
    },
    callByValue = callByValue
  )

  val or = or_impl(callByValue = true)

  def and_impl(callByValue: Boolean) = ComponentImpl(
    "and",
    IS(tyBool, tyBool), tyBool,
    impl = {
      case IS(ValueBool(false), _) => false
      case IS(ValueBool(true), ValueBool(b)) => b
      case _ => ValueError
    },
    callByValue = callByValue
  )

  val and = and_impl(callByValue = true)

  val not = ComponentImpl("not", IS(tyBool), tyBool,
    impl = { case IS(ValueBool(a)) => !a }
  )

  val plus = ComponentImpl("plus", IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => ValueInt(x + y)}
  )

  val noTree = Set(
    // boolean
    T,
    F,
    and,
    or,
    not,
    equal,
    isNil,
    isNonNeg,

    // list
    head,
    tail,
    cons,
    concat,
    nil,

    // integer
    zero,
    inc,
    dec,
    neg,
    length,
    plus,
    div2
  )

  val rules_noTree = Map[ComponentImpl, ReducibleCheck](
    // boolean
    not -> noDirectChild(not),
    and -> reduces(commutative, associative(and), argsDifferent),
    or -> reduces(commutative, associative(or), argsDifferent),
    equal -> reduces(commutative, argsDifferent),
    isNil -> noDirectChild(cons),
    neg -> noDirectChild(neg),

    // list
    length -> noDirectChild(cons),
    concat -> associative(concat),

    //integer
    inc -> noDirectChild(dec),
    dec -> noDirectChild(inc),
    neg -> noDirectChild(neg),
    length -> noDirectChild(cons),
    plus -> reduces(commutative, argsDifferent)
  )

  val createLeaf = ComponentImpl("createLeaf", IS(), tyTree(tyVar(0)),
    impl = { case IS() => ValueTree(BinaryLeaf) }
  )

  val createNode = ComponentImpl(
    "createNode",
    IS(tyVar(0), tyTree(tyVar(0)), tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(v, ValueTree(l), ValueTree(r)) => ValueTree(BinaryNode(v, l, r)) }
  )

  val isLeaf = ComponentImpl(
    "isLeaf",
    IS(tyTree(tyVar(0))),
    tyBool,
    impl = { case IS(ValueTree(t)) => t == BinaryLeaf}
  )

  val treeValue = ComponentImpl(
    "treeValue",
    IS(tyTree(tyVar(0))),
    tyVar(0),
    impl = {
      case IS(ValueTree(BinaryNode(tag, _, _))) => tag
    }
  )

  val treeLeft = ComponentImpl(
    "treeLeft",
    IS(tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(ValueTree(n: BinaryNode[TermValue])) => n.left }
  )

  val treeRight = ComponentImpl(
    "treeRight",
    IS(tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(ValueTree(n: BinaryNode[TermValue])) => n.right }
  )

  val treeComps = Set(
    createLeaf,
    createNode,
    isLeaf,
    treeValue,
    treeLeft,
    treeRight
  )

  val standardComps = noTree ++ treeComps


  def createPair(t1: Type, t2: Type) = ComponentImpl(
    "createPair",
    IS(t1, t2),
    tyPair(t1, t2),
    { case IS(v1, v2) => (v1, v2) }
  )

  val fst = ComponentImpl(
    "fst",
    IS(tyPair(tyVar(0), tyVar(1))),
    tyVar(0),
    { case IS(ValuePair(v)) => v._1 }
  )

  val snd = ComponentImpl(
    "snd",
    IS(tyPair(tyVar(0), tyVar(1))),
    tyVar(1),
    { case IS(ValuePair(v)) => v._2 }
  )

  def pairComps(t1: Type, t2: Type) = Set(
    createPair(t1,t2),
    fst,
    snd
  )

  val insert = ComponentImpl(
    "insert",
    IS(tyList(tyVar(0)), tyInt, tyVar(0)), tyList(tyVar(0)),
    impl = {
      case IS(ValueList(xs), ValueInt(i), v) =>
        val (l,r) = xs.splitAt(i)
        l ++ (v::r)
    }
  )

  val reverse = ComponentImpl(
    "reverse",
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
      "stutter",
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
      "cartesian",
      IS(tyList(tyVar(0)), tyList(tyVar(1))), tyList(tyPair(tyVar(0), tyVar(1))),
      impl = {
        case IS(ValueList(xs), ValueList(ys)) =>
          ValueList(cardF(xs,ys).map(ValuePair))
      }
    )
  }

  val squareList = ComponentImpl(
    "squareList",
    IS(tyInt), tyList(tyInt),
    impl = {
      case IS(ValueInt(n)) =>
        (0 to n).map(x => x* x).toList
    }
  )

  val times = ComponentImpl(
    "times",
    IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => ValueInt(x * y)}
  )

  val div = ComponentImpl(
    "div",
    IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => if(y == 0) ValueError else ValueInt(x / y)}
  )

  val timesAndDiv = Set(
    CommonComps.times,
    CommonComps.div
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

    ComponentImpl("fib", IS(tyInt), tyInt,
      impl = { case IS(ValueInt(n)) => fibF(n)}
    )
  }

  val modulo = {
    ComponentImpl(
      "modulo",
      IS(tyInt, tyInt), tyInt, impl = {
      case IS(ValueInt(a), ValueInt(b)) =>
        if(b == 0) ValueError else a % b
    })
  }

  val gcd = {
    def gcdF(a: Int, b: Int): Int = {
      if(b == 0) a
      else gcdF(b, a % b)
    }

    ComponentImpl("gcd", IS(tyInt, tyInt), tyInt,
      impl = { case IS(ValueInt(a), ValueInt(b)) => gcdF(a,b)
      }
    )
  }

  val compress = {
    def f[A](xs: List[A]): List[A] = xs match {
      case a::b::tail => if(a==b) f(b::tail) else a :: f(b::tail)
      case _ => xs
    }

    ComponentImpl(
      "compress",
      IS(tyList(tyVar(0))), tyList(tyVar(0)),
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

    ComponentImpl("nodesAtLevel", IS(tyTree(tyVar(0)), tyInt), tyTree(tyVar(0)),
      impl = { case IS(ValueTree(tree), ValueInt(level)) =>
        impl(tree, level)
      }
    )
  }

  val contains = {
    ComponentImpl("contains", IS(tyList(tyVar(0)), tyVar(0)), tyBool, impl = {
      case IS(ValueList(xs), x) => xs.contains(x)
    })
  }

  /** Remove duplicate elements from a list. */
  val dedup = {
    def f[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case x::tail =>
        val t = f(tail)
        if(t contains x) t else x :: t
    }

    ComponentImpl("dedup", IS(tyList(tyVar(0))), tyList(tyVar(0)), impl = {
      case IS(ValueList(xs)) => f(xs)
    })
  }

  val dropLast =
    ComponentImpl("dropLast", IS(tyList(tyVar(0))), tyList(tyVar(0)), impl = {
      case IS(ValueList(xs)) =>  xs.dropRight(1)
    })


  /** Remove the odd numbers from a list. */
  val evens = {
    def impl[A](xs: List[A], isEven: Boolean): List[A] = xs match {
      case Nil => Nil
      case x::tail =>
        if(isEven) x::impl(tail, !isEven) else impl(tail, !isEven)
    }
    ComponentImpl("evens", IS(tyList(tyVar(0))), tyList(tyVar(0)), impl = {
      case IS(ValueList(xs)) => impl(xs, isEven = true)
    })
  }

  /** Insert a tree (2n arg) under each leaf of another tree (1st arg) */
  val tConcat = {
    def impl[A](baseTree: BinaryTree[A], insertTree: BinaryTree[A]): BinaryTree[A] = baseTree match {
      case BinaryNode(tag, left, right) => BinaryNode(tag, impl(left, insertTree), impl(right, insertTree))
      case BinaryLeaf => insertTree
    }

    ComponentImpl("tConcat", IS(tyTree(tyVar(0)), tyTree(tyVar(0))), tyTree(tyVar(0)), impl = {
      case IS(ValueTree(t1), ValueTree(t2)) => impl(t1,t2)
    })
  }
}
