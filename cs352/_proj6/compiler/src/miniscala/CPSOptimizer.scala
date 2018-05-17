package miniscala

import scala.collection.mutable.{ Map => MutableMap }

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    val l = fixedPoint(simplifiedTree, 8) { t => inline(t, maxSize) };
    //print(s"tree: ${l}");
    l
  }

  /* Counts how many times a symbol is encountered as an applied function,
   * and how many as a value
   */
  private case class Count(applied: Int = 0, asValue: Int = 0)

  /* Local state of the optimization
   * Note: To update the state, use the with* methods
   */
  private case class State(
    /* How many times a symbol is encountered in the Tree. Note: The
     * census for the whole program gets calculated once in the
     * beginning, and passed to the initial state.
     */
    census: Map[Name, Count],
    // Name substitution that needs to be applied to the current tree
    subst: Substitution[Name] = Substitution.empty,
    // Names that have a constant value
    lEnv: Map[Name, Literal] = Map.empty,
    // The inverse of lEnv
    lInvEnv: Map[Literal, Name] = Map.empty,
    // A known block mapped to its tag and length
    bEnv: Map[Name, (Literal, Name)] = Map.empty,
    // ((p, args) -> n2) is included in eInvEnv iff n2 == p(args)
    // Note: useful for common-subexpression elimination
    eInvEnv: Map[(ValuePrimitive, Seq[Name]), Name] = Map.empty,
    // Continuations that will be inlined
    cEnv: Map[Name, CntDef] = Map.empty,
    // Functions that will be inlined
    fEnv: Map[Name, FunDef] = Map.empty) {

    // Checks whether a symbol is dead in the current state
    def dead(s: Name): Boolean =
      census get s map (_ == Count(applied = 0, asValue = 0)) getOrElse true
    // Checks whether a symbols is applied exactly once as a function
    // in the current State, and never used as a value
    def appliedOnce(s: Name): Boolean =
      census get s map (_ == Count(applied = 1, asValue = 0)) getOrElse false

    // Addas a substitution to the state
    def withSubst(from: Name, to: Name): State =
      copy(subst = subst + (from -> to))
    // Adds a Seq of substitutions to the state
    def withSubst(from: Seq[Name], to: Seq[Name]): State =
      copy(subst = subst ++ (from zip to))

    // Adds a constant to the State
    def withLit(name: Name, value: Literal) =
      copy(lEnv = lEnv + (name -> value), lInvEnv = lInvEnv + (value -> name))
    // Adds a block to the state
    def withBlock(name: Name, tag: Literal, size: Name) =
      copy(bEnv = bEnv + (name -> (tag, size)))
    // Adds a primitive assignment to the state
    def withExp(name: Name, prim: ValuePrimitive, args: Seq[Name]) =
      copy(eInvEnv = eInvEnv + ((prim, args) -> name))
    // Adds an inlinable continuation to the state
    def withCnt(cnt: CntDef) =
      copy(cEnv = cEnv + (cnt.name -> cnt))
    // Adds a Seq of inlinable continuations to the state
    def withCnts(cnts: Seq[CntDef]) =
      (this /: cnts) (_.withCnt(_))
    // Adds an inlinable function to the state
    def withFun(fun: FunDef) =
      copy(fEnv = fEnv + (fun.name -> fun))
    // Adds a Seq of inlinable functions to the state
    def withFuns(funs: Seq[FunDef]) =
      (this /: funs) (_.withFun(_))
    /*
     * The same state, with emply inverse environments.
     * Use this when entering a new FunDef, because assigned Name's may
     * come out of scope during hoisting.
     */
    def withEmptyInvEnvs =
      copy(lInvEnv = Map.empty, eInvEnv = Map.empty)
  }


  /*
  ██ ██   ██ ███████ ██      ██████  ███████ ██████  ███████
  ██ ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
  ██ ███████ █████   ██      ██████  █████   ██████  ███████
  ██ ██   ██ ██      ██      ██      ██      ██   ██      ██
  ██ ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
  */

  private def print(s: String) = {
    println("===========");
    println(s);
    println("===========");
  }

  private def allInlEnv(args: Seq[Name], s: State): Boolean = {
    var f = true;
    def test(n: Name) = if(!(s.lEnv contains n)) f = false;
    args foreach test;
    f
  }

  private def getLits(args: Seq[Name], s: State): Seq[Literal] = {
    var lits = Seq[Literal]();
    for(a <- args) lits = lits :+ s.lEnv(a)
    lits
  }

  var counter = 0;

  // Shrinking optimizations
  private def shrink(tree: Tree): Tree = {
    def shrinkT(tree: Tree)(implicit s: State): Tree = tree match {
      // TODO

      /*
      ██  ██      ███████ ████████ ██
      ██  ██      ██         ██    ██
      ██  ██      █████      ██    ██
      ██  ██      ██         ██    ██
      ██  ███████ ███████    ██    ███████
      */

      case LetL(n, v, body) =>
        // Dead Code
        if(s.dead(n)){
          shrinkT(body)(s)
        }

        // CSE
        else if(s.lInvEnv contains v){
          val s2 = s.withSubst(n, s.lInvEnv(v));
          shrinkT(body subst s2.subst)(s2)
        }

        else {
          LetL(n, v, shrinkT(body)(s.withLit(n, v)))
        }

      /*
      ██ ██      ███████ ████████ ██████
      ██ ██      ██         ██    ██   ██
      ██ ██      █████      ██    ██████
      ██ ██      ██         ██    ██
      ██ ███████ ███████    ██    ██
      */

      case LetP(n, p, args: Seq[Name], body) =>
        val imp = impure(p);
        val unst = unstable(p);
        val len = args.length;

        // Dead Code
        if(s.dead(n) && !imp){
          shrinkT(body)(s)
        }

        // CSE
        else if((s.eInvEnv contains (p, args)) && !unst && !imp){
          val s2 = s.withSubst(n, s.eInvEnv((p, args)));
          shrinkT(body subst s2.subst)(s2)
        }

        // remove ID
        else if(p == identity) {
          val s2 = s.withSubst(n, args(0));
          shrinkT(body subst s2.subst)(s2)
        }

        // block alloc
        else if(blockAllocTag.isDefinedAt(p)){
          val tag = blockAllocTag(p);
          val size = args(0);
          val s2 = s.withBlock(n, tag, size);
          LetP(n, p, args, shrinkT(body)(s2))
        }

        // block tag
        else if(p == blockTag && (s.bEnv contains args(0))){
          shrinkT(LetL(n, s.bEnv(args(0))._1, body));
        }

        // block len
        else if(p == blockLength && (s.bEnv contains args(0))){
          shrinkT(LetL(n, s.lEnv(s.bEnv(args(0))._2), body));
        }

        else if(!imp && !unst){
          if(len == 2) {
            if(allInlEnv(args, s)){
              // Constant Folding
              val lits = getLits(args, s);
              val v = vEvaluator((p, lits));
              shrinkT(LetL(n, v, body))(s)
            }

            // Left
            else if(s.lEnv contains(args(0))){
              // Neutral
              if (leftNeutral((s.lEnv(args(0)), p))) {
                val s2 = s.withSubst(n, args(1));
                shrinkT(body subst s2.subst)(s2)
              }
              //Absorbing
              else if (leftAbsorbing(s.lEnv(args(0)), p)) {
                val s2 = s.withSubst(n, args(0));
                shrinkT(body subst s2.subst)(s2)
              }
              else LetP(n, p, args, shrinkT(body)(s.withExp(n, p, args)))
            }

            // Right
            else if(s.lEnv contains args(1)) {
              // Neutral
              if (rightNeutral((p, s.lEnv(args(1))))) {
                val s2 = s.withSubst(n, args(0));
                shrinkT(body subst s2.subst)(s2)
              }
              //Absorbing
              else if(rightAbsorbing((p, s.lEnv(args(1))))){
                val s2 = s.withSubst(n, args(1));
                shrinkT(body subst s2.subst)(s2)
              }
              else LetP(n, p, args, shrinkT(body)(s.withExp(n, p, args)))
            }

            // Same Arg
            else if(args(0) == args(1) && sameArgReduce.isDefinedAt(p)){
              shrinkT(LetL(n, sameArgReduce(p), body))
            }
            else LetP(n, p, args, shrinkT(body)(s.withExp(n, p, args)))
          }

          else LetP(n, p, args, shrinkT(body)(s.withExp(n, p, args)))
        }

        else LetP(n, p, args, shrinkT(body)(s.withExp(n, p, args)))

        /*
        ██ ██      ███████ ████████ ███████
        ██ ██      ██         ██    ██
        ██ ██      █████      ██    █████
        ██ ██      ██         ██    ██
        ██ ███████ ███████    ██    ██
        */

        case LetF(funs, body) =>
          // Dead Code
          val funNames = funs map (_.name)
          var nFuns = Seq[FunDef]();
          var s2 = s;

          for(f <- funs) {
            if(!(s.dead(f.name))) {
               f.body match {
                case AppF(name, f.retC, f.args) =>
                  s2 = s2.withSubst(f.name, name);
                case _ =>
                  if(!(s.appliedOnce(f.name))){
                    val f2 = FunDef(f.name, f.retC, f.args, shrinkT(f.body subst s2.subst)(s2.withEmptyInvEnvs))
                    nFuns = nFuns :+ f2;
                  }
               }
               if(s.appliedOnce(f.name)) {
                 s2 = s2.withFun(f);
               }
            }
          }

          if(nFuns.length == 0){
            shrinkT(body subst s2.subst)(s2)
          }
          else LetF(nFuns, shrinkT(body subst s2.subst)(s2)) subst s2.subst

        /*
        ██ ██      ███████ ████████  ██████
        ██ ██      ██         ██    ██
        ██ ██      █████      ██    ██
        ██ ██      ██         ██    ██
        ██ ███████ ███████    ██     ██████
        */

        case LetC(cnts, body) =>
          // Dead Code
          val cntNames = cnts map (_.name)
          var nCnts = Seq[CntDef]();
          var s2 = s;

          for(c <- cnts) {
            if(!(s.dead(c.name))) {
              c.body match {
               case AppC(name, c.args) =>
                 s2 = s2.withSubst(c.name, name);
               case _ =>
                 val c2 = CntDef(c.name, c.args, shrinkT(c.body subst s2.subst)(s2.withEmptyInvEnvs))
                 nCnts = nCnts :+ c2;
              }
              if(s.appliedOnce(c.name)) {
                s2 = s2.withCnt(c);
              }
            }
          }

          if(nCnts.length == 0) {
            shrinkT(body subst s2.subst)(s2)
          }
          else LetC(nCnts, shrinkT(body subst s2.subst)(s2)) subst s2.subst

        /*
        ██ ██████ ███████
        ██   ██   ██
        ██   ██   █████
        ██   ██   ██
        ██ ██████ ██
        */

        case If(cond, args: Seq[Name], thenC, elseC) =>
          if(args.length == 2 /*&& sameArgReduceC.isDefinedAt(cond)*/){
            // Constant Folding
            if(args(0) == args(1) && sameArgReduceC(cond)) {
              AppC(thenC, Seq[Name]());
            }

            else if(args(0) != args(1) && !sameArgReduceC(cond)){
              AppC(elseC, Seq[Name]());
            }
            else If(cond, args, thenC, elseC)
          }

          else if(allInlEnv(args, s)){
            val lits = getLits(args, s);
            if(cEvaluator.isDefinedAt((cond, lits))){
              val cond_res = cEvaluator((cond, lits));
              if(cond_res)
              AppC(thenC, Seq[Name]())
              else
              AppC(elseC, Seq[Name]())
            }
            else If(cond, args, thenC, elseC)
          }

          else If(cond, args, thenC, elseC)

        /*
        ██  █████  ██████  ██████   ██████
        ██ ██   ██ ██   ██ ██   ██ ██
        ██ ███████ ██████  ██████  ██
        ██ ██   ██ ██      ██      ██
        ██ ██   ██ ██      ██       ██████
        */

        case AppC(name, args: Seq[Name]) =>
          // Shrinking Inlining
          if(s.cEnv contains name) {
            val cnt = s.cEnv(name);
            var s2 = s;
            // for(a <- c.args) {
            //   val b = Symbol.fresh(s"${a}");
            //   s2 = s2.withSubst(a, b);
            // }
            s2 = s2.withSubst(cnt.args, args);
            shrinkT(cnt.body subst s2.subst)(s2)
          }

          else AppC(name, args)

        /*
        ██  █████  ██████  ██████  ███████
        ██ ██   ██ ██   ██ ██   ██ ██
        ██ ███████ ██████  ██████  █████
        ██ ██   ██ ██      ██      ██
        ██ ██   ██ ██      ██      ██
        */

        case AppF(name, retC, args: Seq[Name]) =>
          // Shrinking Inlining
          if(s.fEnv contains name) {
            val fun = s.fEnv(name);
            var s2 = s.withSubst(fun.retC, retC);
            // for(a <- fun.args){
            //   val b = Symbol.fresh(s"${a}");
            //   s2 = s2.withSubst(a, b);
            // }
            s2 = s2.withSubst(fun.args, args);
            shrinkT(fun.body subst s2.subst)(s2)
          }

          else AppF(name, retC, args)

      case _ => tree
    }


    shrinkT(tree)(State(census(tree)))
  }

  /*
      ██ ███    ██ ██      ██ ███    ██ ███████
      ██ ████   ██ ██      ██ ████   ██ ██
      ██ ██ ██  ██ ██      ██ ██ ██  ██ █████
      ██ ██  ██ ██ ██      ██ ██  ██ ██ ██
      ██ ██   ████ ███████ ██ ██   ████ ███████
  */


  // (Non-shrinking) inlining

  private def inline(tree: Tree, maxSize: Int): Tree = {

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length) { case (i, tree) =>
      // Size limit for function and continuation to be inlined
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {
        // TODO

        /*
        ██ ██      ███████ ████████ ███████
        ██ ██      ██         ██    ██
        ██ ██      █████      ██    █████
        ██ ██      ██         ██    ██
        ██ ███████ ███████    ██    ██
        */

        case LetF(funs, body) =>
          val funNames = funs map (_.name)
          var nFuns = Seq[FunDef]();
          var s2 = s;

          for(f <- funs) {
            if(!(s.dead(f.name))) {
              val sze = size(body);
               if(sze <= funLimit && !(s.fEnv contains f.name)) {
                 s2 = s2.withFun(f);
               }
              val f2 = FunDef(f.name, f.retC, f.args, inlineT(f.body)(s2.withEmptyInvEnvs))
              nFuns = nFuns :+ f2;
            }
          }

          LetF(nFuns, inlineT(body)(s2))

        /*
        ██ ██      ███████ ████████  ██████
        ██ ██      ██         ██    ██
        ██ ██      █████      ██    ██
        ██ ██      ██         ██    ██
        ██ ███████ ███████    ██     ██████
        */

        case LetC(cnts, body) =>
          val cntNames = cnts map (_.name)
          var nCnts = Seq[CntDef]();
          var s2 = s;

          for(c <- cnts) {
            if(c.name != Nil && !(s.dead(c.name))) {
              val sze = size(body);
               if(sze <= cntLimit && !(s.cEnv contains c.name)) {
                 s2 = s2.withCnt(c);
               }
              val c2 = CntDef(c.name, c.args, inlineT(shrink(c.body))(s2.withEmptyInvEnvs))
              nCnts = nCnts :+ c2;
            }
          }

          if(nCnts.length == 0){
            inlineT(body)(s2)
          }
          else LetC(nCnts, inlineT(body)(s2))

        /*
        ██  █████  ██████  ██████   ██████
        ██ ██   ██ ██   ██ ██   ██ ██
        ██ ███████ ██████  ██████  ██
        ██ ██   ██ ██      ██      ██
        ██ ██   ██ ██      ██       ██████
        */

        case AppC(name, args: Seq[Name]) =>
          // // Inlining
          if(s.cEnv contains name) {
            val cnt = s.cEnv(name);
            var nArgs = Seq[Name]();
            for(a <- cnt.args) {
              nArgs :+ Symbol.fresh(s"${a}");
            }
            val s2 = s.withSubst(args, nArgs);
            inlineT(shrink(cnt.body subst s2.subst))(s2)
            //cnt.body //subst s2.subst
          }

          else AppC(name, args)

        /*
        ██  █████  ██████  ██████  ███████
        ██ ██   ██ ██   ██ ██   ██ ██
        ██ ███████ ██████  ██████  █████
        ██ ██   ██ ██      ██      ██
        ██ ██   ██ ██      ██      ██
        */

        case AppF(name, retC, args: Seq[Name]) =>
          // Inlining

          // if(s.fEnv contains name) {
          //   val fun = s.fEnv(name);
          //   val nRetC = Symbol.fresh(s"${fun.retC}");
          //   var s2 = s.withSubst(retC, nRetC);
          //   var nArgs = Seq[Name]();
          //   for(a <- fun.args){
          //     val b = Symbol.fresh(s"${a}");
          //     nArgs :+ b;
          //   }
          //   s2 = s2.withSubst(args, nArgs);
          //   inlineT(fun.body subst s2.subst)(s2)
          // }
          // else AppF(name, retC, args)


          if(s.fEnv contains name) {
            val fun = s.fEnv(name);
            val nRetC = Symbol.fresh(s"${fun.retC}");
            var s2 = s.withSubst(retC, nRetC);
            // print(s"adding ${nRetC} for ${retC}");
            var nArgs = Seq[Name]();
            for(a <- fun.args){
              val b = Symbol.fresh(s"${a}");
              nArgs :+ b;
            }
            // for(a <- fun.args){
            //   val b = Symbol.fresh(s"${a}");
            //   s2 = s2.withSubst(a, b);
            //   print(s"adding ${b} for ${a}");
            // }
            s2 = s2.withSubst(args, nArgs);
            //inlineT(fun.body subst s2.subst)(s2)
            shrink(fun.body subst s2.subst)
          }

          else AppF(name, retC, args)


        /*
             ██████  ████████ ██   ██ ███████ ██████  ███████
            ██    ██    ██    ██   ██ ██      ██   ██ ██
            ██    ██    ██    ███████ █████   ██████  ███████
            ██    ██    ██    ██   ██ ██      ██   ██      ██
             ██████     ██    ██   ██ ███████ ██   ██ ███████
        */

        case LetL(name, value, body) => LetL(name, value, inlineT(body)(s.withLit(name, value)))
        case LetP(name, prim, args, body) => LetP(name, prim, args, inlineT(body)(s))
        //case If(cond, args, thenC, elseC) => tree
        //case Halt(name) => tree
        case _ => tree
      }

      (i + 1, fixedPoint(inlineT(tree)(State(census(tree))))(shrink))
    }

    trees.takeWhile{ case (_, tree) => size(tree) <= maxSize }.last._2
  }

  // Census computation
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]()
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(applied = currCount.applied + 1)
      rhs remove symbol foreach addToCensus
    }

    def incValUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(asValue = currCount.asValue + 1)
      rhs remove symbol foreach addToCensus
    }

    def addToCensus(tree: Tree): Unit = (tree: @unchecked) match {
      case LetL(_, _, body) =>
        addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case AppC(cnt, args) =>
        incAppUse(cnt); args foreach incValUse
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt(arg) =>
        incValUse(arg)
    }

    addToCensus(tree)
    census.toMap
  }

  private def sameLen(formalArgs: Seq[Name], actualArgs: Seq[Name]): Boolean =
    formalArgs.length == actualArgs.length

  private def size(tree: Tree): Int = (tree: @unchecked) match {
    case LetL(_, _, body) => size(body) + 1
    case LetP(_, _, _, body) => size(body) + 1
    case LetC(cs, body) => (cs map { c => size(c.body) }).sum + size(body)
    case LetF(fs, body) => (fs map { f => size(f.body) }).sum + size(body)
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) => 1
  }

  // Returns whether a ValuePrimitive has side-effects
  protected val impure: ValuePrimitive => Boolean
  // Returns whether different applications of a ValuePrimivite on the
  // same arguments may yield different results
  protected val unstable: ValuePrimitive => Boolean
  // Extracts the tag from a block allocation primitive
  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal]
  // Returns true for the block tag primitive
  protected val blockTag: ValuePrimitive
  // Returns true for the block length primitive
  protected val blockLength: ValuePrimitive
  // Returns true for the identity primitive
  protected val identity: ValuePrimitive

  // ValuePrimitives with their left-neutral elements
  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-neutral elements
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with their left-absorbing elements
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-absorbing elements
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with the value equal arguments reduce to
  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal]
  // TestPrimitives with the (boolean) value equal arguments reduce to
  protected val sameArgReduceC: TestPrimitive => Boolean
  // An evaluator for ValuePrimitives
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal]
  // An evaluator for TestPrimitives
  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean]
}

/*
██  ██████ ██████  ███████     ██   ██ ██  ██████  ██   ██
██ ██      ██   ██ ██          ██   ██ ██ ██       ██   ██
██ ██      ██████  ███████     ███████ ██ ██   ███ ███████
██ ██      ██           ██     ██   ██ ██ ██    ██ ██   ██
██  ██████ ██      ███████     ██   ██ ██  ██████  ██   ██
*/


object CPSOptimizerHigh extends CPSOptimizer(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => SymbolicCPSTreeModule.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(MiniScalaBlockSet, MiniScalaByteRead, MiniScalaByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    // TODO -- done
    case MiniScalaBlockAlloc(_) | MiniScalaBlockGet | MiniScalaByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case MiniScalaBlockAlloc(tag) => IntLit(tag)
  }
  protected val blockTag: ValuePrimitive = MiniScalaBlockTag
  protected val blockLength: ValuePrimitive = MiniScalaBlockLength

  protected val identity: ValuePrimitive = MiniScalaId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((IntLit(0), MiniScalaIntAdd), (IntLit(1), MiniScalaIntMul), (IntLit(~0), MiniScalaIntBitwiseAnd), (IntLit(0), MiniScalaIntBitwiseOr), (IntLit(0), MiniScalaIntBitwiseXOr))// TODO
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((MiniScalaIntAdd, IntLit(0)), (MiniScalaIntSub, IntLit(0)), (MiniScalaIntMul, IntLit(1)), (MiniScalaIntDiv, IntLit(1)),
            (MiniScalaIntArithShiftLeft, IntLit(0)), (MiniScalaIntArithShiftRight, IntLit(0)),
            (MiniScalaIntBitwiseAnd, IntLit(~0)), (MiniScalaIntBitwiseOr, IntLit(0)), (MiniScalaIntBitwiseXOr, IntLit(0))) // TODO
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((IntLit(0), MiniScalaIntMul), (IntLit(0), MiniScalaIntBitwiseAnd), (IntLit(~0), MiniScalaIntBitwiseOr)) // TODO
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((MiniScalaIntMul, IntLit(0)), (MiniScalaIntBitwiseAnd, IntLit(0)), (MiniScalaIntBitwiseOr, IntLit(~0))) // TODO

  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal] =
    Map(MiniScalaIntSub -> IntLit(0), MiniScalaIntDiv -> IntLit(1), MiniScalaIntMod -> IntLit(0), MiniScalaIntBitwiseXOr -> IntLit(0)) // TODO

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case MiniScalaIntLe | MiniScalaIntGe | MiniScalaEq => true
    case MiniScalaIntLt | MiniScalaIntGt | MiniScalaNe => false
    // TODO
    case _ => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (MiniScalaIntAdd, Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
    // TODO -- done
    case (MiniScalaIntSub, Seq(IntLit(x), IntLit(y))) => IntLit(x - y)
    case (MiniScalaIntMul, Seq(IntLit(x), IntLit(y))) => IntLit(x * y)
    case (MiniScalaIntDiv, Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(Math.floorDiv(x, y))
    case (MiniScalaIntMod, Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(Math.floorMod(x, y))

    case (MiniScalaIntArithShiftLeft, Seq(IntLit(x), IntLit(y))) => IntLit(x << y)
    case (MiniScalaIntArithShiftRight, Seq(IntLit(x), IntLit(y))) => IntLit(x >> y)
    case (MiniScalaIntBitwiseAnd, Seq(IntLit(x), IntLit(y))) => IntLit(x & y)
    case (MiniScalaIntBitwiseOr, Seq(IntLit(x), IntLit(y))) => IntLit(x | y)
    case (MiniScalaIntBitwiseXOr, Seq(IntLit(x), IntLit(y))) => IntLit(x ^ y)

    // Experimental
    case (MiniScalaCharToInt, Seq(CharLit(x))) => IntLit(x.toInt)
    case (MiniScalaIntToChar, Seq(IntLit(x))) => CharLit(x.toChar)
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {

    case (MiniScalaIntP, Seq(IntLit(_))) => true
    // TODO
    case (MiniScalaIntP, Seq(_)) => false
    case (MiniScalaCharP, Seq(CharLit(_))) => true
    case (MiniScalaCharP, Seq(_)) => false
    case (MiniScalaBoolP, Seq(BooleanLit(_))) => true
    case (MiniScalaBoolP, Seq(_)) => false
    case (MiniScalaUnitP, Seq(UnitLit)) => true
    case (MiniScalaUnitP, Seq(_)) => false

    // int cond
    case (MiniScalaIntLt, Seq(IntLit(x), IntLit(y))) => x < y
    case (MiniScalaIntLe, Seq(IntLit(x), IntLit(y))) => x <= y
    case (MiniScalaEq, Seq(IntLit(x), IntLit(y))) => x == y
    case (MiniScalaNe, Seq(IntLit(x), IntLit(y))) => x != y
    case (MiniScalaIntGe, Seq(IntLit(x), IntLit(y))) => x >= y
    case (MiniScalaIntGt, Seq(IntLit(x), IntLit(y))) => x > y

    // bool cond
    case (MiniScalaEq, Seq(BooleanLit(x), BooleanLit(y))) => x == y
    case (MiniScalaNe, Seq(BooleanLit(x), BooleanLit(y))) => x != y

  }
}

object CPSOptimizerLow extends CPSOptimizer(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => SymbolicCPSTreeModuleLow.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(CPSBlockSet, CPSByteRead, CPSByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case CPSBlockAlloc(_) | CPSBlockGet | CPSByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case CPSBlockAlloc(tag) => tag
  }
  protected val blockTag: ValuePrimitive = CPSBlockTag
  protected val blockLength: ValuePrimitive = CPSBlockLength

  protected val identity: ValuePrimitive = CPSId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (1, CPSMul), (~0, CPSAnd), (0, CPSOr), (0, CPSXOr))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), (CPSSub, 0), (CPSMul, 1), (CPSDiv, 1),
        (CPSArithShiftL, 0), (CPSArithShiftR, 0),
        (CPSAnd, ~0), (CPSOr, 0), (CPSXOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd), (~0, CPSOr))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0), (CPSOr, ~0))

  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0, CPSDiv -> 1, CPSMod -> 0, CPSXOr -> 0)

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case CPSLe | CPSGe | CPSEq => true
    case CPSLt | CPSGt | CPSNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (CPSAdd, Seq(x, y)) => x + y
    case (CPSSub, Seq(x, y)) => x - y
    case (CPSMul, Seq(x, y)) => x * y
    case (CPSDiv, Seq(x, y)) if (y != 0) => Math.floorDiv(x, y)
    case (CPSMod, Seq(x, y)) if (y != 0) => Math.floorMod(x, y)

    case (CPSArithShiftL, Seq(x, y)) => x << y
    case (CPSArithShiftR, Seq(x, y)) => x >> y
    case (CPSAnd, Seq(x, y)) => x & y
    case (CPSOr, Seq(x, y)) => x | y
    case (CPSXOr, Seq(x, y)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {

    case (CPSLt, Seq(x, y)) => x < y
    case (CPSLe, Seq(x, y)) => x <= y
    case (CPSEq, Seq(x, y)) => x == y
    case (CPSNe, Seq(x, y)) => x != y
    case (CPSGe, Seq(x, y)) => x >= y
    case (CPSGt, Seq(x, y)) => x > y
  }
}
