package escher

import escher.CommonComps.ReducibleCheck
import escher.Synthesis.ArgList

/** An Exception which will not be caught by ComponentImpl.execute */
case class ExecutionError(msg: String) extends Exception {
  override def getMessage: String = msg

  override def getLocalizedMessage: String = msg
}


case class ComponentImpl(inputTypes: IS[Type], returnType: Type,
                         impl: PartialFunction[IS[TermValue],TermValue],
                         callByValue: Boolean = true){

  def shiftTypeId(amount: Int) = ComponentImpl(inputTypes.map(_.shiftId(amount)), returnType.shiftId(amount), impl)

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
                    compMap: Map[String, ComponentImpl],
                    argListCompare: (ArgList,ArgList) => Boolean,
                    body: Term, debug: Boolean = false
                   ): ComponentImpl = {

    import collection.mutable

    def impl(lastArg: Option[ArgList], buffer: mutable.Map[ArgList, TermValue]): ComponentImpl = {
      ComponentImpl(inputTypes, returnType, {
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

    def noDirectChild(childName: String): ReducibleCheck = ReducibleCheck(1, {
      case IS(Term.Component(n, _)) => n == childName
    })

    val argsDifferent = ReducibleCheck(2, {
      case IS(t1, t2) => t1 == t2
    })

    def associative(opName: String) = ReducibleCheck(2, {
      case IS(_, Term.Component(n, _)) => n == opName
    })
  }
  import ReducibleCheck._

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
    impl = { case IS(ValueList(_::xs)) => ValueList(xs)}
  )

  val cons = ComponentImpl(IS(tyVar(0), TList of tyVar(0)), TList of tyVar(0),
    impl = { case IS(x, ValueList(x2)) => ValueList(x :: x2)}
  )

  val head = ComponentImpl(IS(TList of tyVar(0)), tyVar(0),
    impl = { case IS(ValueList(x::_)) => x }
  )

  val emptyList = ComponentImpl(IS(), tyList(tyVar(0)),
    impl = { case IS() => listValue() }
  )

  val concat = ComponentImpl(IS(TList of tyVar(0), tyList(tyVar(0))), TList of tyVar(0),
    impl = { case IS(ValueList(xs), ValueList(ys)) => ValueList(xs ++ ys)}
  )

  val T = ComponentImpl(IS(), tyBool,
    impl = { case IS() => true }
  )

  val F = ComponentImpl(IS(), tyBool,
    impl = { case IS() => false }
  )

  val equal = ComponentImpl(IS(tyVar(0), tyVar(0)), tyBool,
    impl = { case IS(a,b) => ValueBool(a == b) }
  )

  def or(callByValue: Boolean) = ComponentImpl(IS(tyBool, tyBool), tyBool,
    impl = {
      case IS(ValueBool(true), _) => true
      case IS(ValueBool(false), ValueBool(b)) => b
      case _ => ValueError
    },
    callByValue = callByValue
  )

  def and(callByValue: Boolean) = ComponentImpl(IS(tyBool, tyBool), tyBool,
    impl = {
      case IS(ValueBool(false), _) => false
      case IS(ValueBool(true), ValueBool(b)) => b
      case _ => ValueError
    },
    callByValue = callByValue
  )

  val not = ComponentImpl(IS(tyBool), tyBool,
    impl = { case IS(ValueBool(a)) => !a }
  )

  val plus = ComponentImpl(IS(tyInt, tyInt), tyInt,
    impl = { case IS(ValueInt(x), ValueInt(y)) => ValueInt(x + y)}
  )

  val noTree = Map(
    // boolean
    "T" -> T,
    "F" -> F,
    "and" -> and(callByValue = true),
    "or" -> or(callByValue = true),
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

  val rules_noTree = Map[String, ReducibleCheck](
    // boolean
    "not" -> noDirectChild("not"),
    "and" -> reduces(commutative, associative("and"), argsDifferent),
    "or" -> reduces(commutative, associative("or"), argsDifferent),
    "equal" -> reduces(commutative, argsDifferent),
    "isEmpty" -> noDirectChild("cons"),
    "isNonNeg" -> noDirectChild("neg"),

    // list
    "tail" -> noDirectChild("cons"),
    "concat" -> associative("concat"),

    //integer
    "inc" -> noDirectChild("dec"),
    "dic" -> noDirectChild("inc"),
    "neg" -> noDirectChild("nec"),
    "length" -> noDirectChild("cons"),
    "plus" -> reduces(commutative, argsDifferent)
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
    impl = {
      case IS(ValueTree(BinaryNode(tag, _, _))) => tag
    }
  )

  val treeLeft = ComponentImpl(
    IS(tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(ValueTree(n: BinaryNode[TermValue])) => n.left }
  )

  val treeRight = ComponentImpl(
    IS(tyTree(tyVar(0))),
    tyTree(tyVar(0)),
    impl = { case IS(ValueTree(n: BinaryNode[TermValue])) => n.right }
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

  val contains = {
    ComponentImpl(IS(tyList(tyVar(0)), tyVar(0)), tyBool, impl = {
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

    ComponentImpl(IS(tyList(tyVar(0))), tyList(tyVar(0)), impl = {
      case IS(ValueList(xs)) => f(xs)
    })
  }

  val dropLast =
    ComponentImpl(IS(tyList(tyVar(0))), tyList(tyVar(0)), impl = {
      case IS(ValueList(xs)) =>  xs.dropRight(1)
    })


  /** Remove the odd numbers from a list. */
  val evens = {
    def impl[A](xs: List[A], isEven: Boolean): List[A] = xs match {
      case Nil => Nil
      case x::tail =>
        if(isEven) x::impl(tail, !isEven) else impl(tail, !isEven)
    }
    ComponentImpl(IS(tyList(tyVar(0))), tyList(tyVar(0)), impl = {
      case IS(ValueList(xs)) => impl(xs, isEven = true)
    })
  }

  /** Insert a tree (2n arg) under each leaf of another tree (1st arg) */
  val tConcat = {
    def impl[A](baseTree: BinaryTree[A], insertTree: BinaryTree[A]): BinaryTree[A] = baseTree match {
      case BinaryNode(tag, left, right) => BinaryNode(tag, impl(left, insertTree), impl(right, insertTree))
      case BinaryLeaf => insertTree
    }

    ComponentImpl(IS(tyTree(tyVar(0)), tyTree(tyVar(0))), tyTree(tyVar(0)), impl = {
      case IS(ValueTree(t1), ValueTree(t2)) => impl(t1,t2)
    })
  }
}
