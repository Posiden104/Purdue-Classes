package miniscala

import Language._

/**
 * This interpreter specifies the semantics of our
 * programming language.
 *
 * The evaluation of each node returns a value.
 */
object ValueInterpreter extends (Tree => Int) with BugReporter {
  /*
   * Values for primitives operators. Already stored in the environment.
   */
  val primitives = Map[String, BoxedVal](
    "putchar" -> BoxedVal(Primitive("putchar")),
    "getchar" -> BoxedVal(Primitive("getchar"))
  )

  /*
   * Definition of the values of our language
   *
   * We can return constant Int, Boolean, Unit or Array.
   * We can also return Function. Primitive is a special
   * kind of function.
   */

  abstract class Val
  case class Cst(x: Any) extends Val {
    override def toString = if (x != null) x.toString else "null"
  }
  case class Func(args: List[String], fbody: Tree, var env: Env) extends Val {
    // Add all the value into the existing environment
    def withVals(list: List[(String,Val)]) = env = env.withVals(list)
    override def toString = s"(${args mkString ","}) => $fbody"
  }
  case class Primitive(name: String) extends Val

  case class BoxedVal(var v: Val)

  /**
   * Env of the interpreter. Keeps track of the value
   * of each variable defined.
   */
  case class Env(vars: Map[String, BoxedVal] = primitives) {
    def undef(name: String) =
      BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")

    /*
     * Return a copy of the current state plus an immutable
     * variable 'name' of value 'v'
     */
    def withVal(name: String, v: Val): Env = {
      copy(vars = vars + (name -> BoxedVal(v)))
    }

    /*
     * Return a copy of the current state plus all the immutables
     * variable in list.
     */
    def withVals(list: List[(String,Val)]): Env = {
      copy(vars = vars ++ (list.map {case (n, v) => n -> BoxedVal(v) }))
    }

    /*
     * Update the variable 'name'
     * Return the new value of the variable
     */
    def updateVar(name: String, v: Val): Val = {
      if (vars.contains(name))
        vars(name).v = v
      else
        undef(name)
      v
    }

    /*
     * Return the value of the variable 'name'
     */
    def apply(name: String): Val = {
      if (vars.contains(name))
        vars(name).v
      else
        undef(name)
    }
  }

  /*
   * Compute and return the result of the unary
   * operation 'op' on the value 'v'
   */
  def evalUn(op: String)(v: Val) = (op, v) match {
    case ("-", Cst(v: Int)) => Cst(-v)
    case ("+", Cst(_: Int)) => v
    case _ => BUG(s"unary operator $op undefined")
  }

  /*
   * Compute and return the result of the binary
   * operation 'op' on the value 'v' and 'w'
   * Note: v op w
   */
  def evalBin(op: String)(v: Val, w: Val) = (op, v, w) match {
    case ("-", Cst(v: Int), Cst(w: Int)) => Cst(v-w)
    case ("+", Cst(v: Int), Cst(w: Int)) => Cst(v+w)
    case ("*", Cst(v: Int), Cst(w: Int)) => Cst(v*w)
    case ("/", Cst(v: Int), Cst(w: Int)) => Cst(v/w)
    case ("==", Cst(v: Int), Cst(w: Int)) => Cst(v == w)
    case ("!=", Cst(v: Int), Cst(w: Int)) => Cst(v != w)
    case ("<=", Cst(v: Int), Cst(w: Int)) => Cst(v <= w)
    case (">=", Cst(v: Int), Cst(w: Int)) => Cst(v >= w)
    case ("<" , Cst(v: Int), Cst(w: Int)) => Cst(v < w)
    case (">" , Cst(v: Int), Cst(w: Int)) => Cst(v > w)
    case ("block-get", Cst(arr: Array[Any]), Cst(i: Int)) =>
      if (arr(i) == null)
        BUG(s"uninitialized memory")
      Cst(arr(i))
    case _ => BUG(s"binary operator $op undefined")
  }

  /*
   * Compute and return the result of the ternary
   * operations 'op' on the value 'v', 'w' and 'z'
   */
  def evalTer(op: String)(v: Val, w: Val, z: Val) = (op, v, w, z) match {
    case ("block-set", Cst(arr: Array[Any]), Cst(i: Int), Cst(x)) => Cst(arr(i) = x)
    case _ => BUG(s"ternary operator $op undefined")
  }

  def evalPrim(op: String)(eargs: List[Val]) = eargs match {
    case List(v, w, z) => evalTer(op)(v, w, z)
    case List(v, w)    => evalBin(op)(v, w)
    case List(v)       => evalUn(op)(v)
    case _ => BUG(s"no prim with ${eargs.length} arguments")
  }

  /*
   * Evaluate the AST starting with an empty Env
   */
  def apply(tree: Tree) = {
    val Cst(x: Int) = eval(tree)(Env())
    x
  }

  /*
   * Evaluate the AST within the environment 'env'
   */
  def eval(tree: Tree)(env: Env): Val = tree match {
    case Lit(x) => Cst(x)
    case Prim(op, args) =>
      val eargs = args map { arg => eval(arg)(env) }
      evalPrim(op)(eargs)
    case Let(x, tp, a, b) =>
      eval(b)(env.withVal(x, eval(a)(env)))
    case Ref(x) =>
      env(x)
    case If(cond, tBranch, eBranch) =>
      val Cst(v: Boolean) = eval(cond)(env)
      if (v)
        eval(tBranch)(env)
      else
        eval(eBranch)(env)
    case VarDec(x, tp, rhs, body) =>
      eval(body)(env.withVal(x, eval(rhs)(env)))
    case VarAssign(x, rhs) =>
      env.updateVar(x, eval(rhs)(env))
    case While(cond, lBody, body) =>
      while (eval(cond)(env) == Cst(true)) {
        eval(lBody)(env)
      }
      eval(body)(env)
    case FunDef(_, args, _, fbody) =>
      Func(args map { arg => arg.name }, fbody, env)
    case LetRec(funs, body) =>
      // Evaluate all functions
      val funcs = funs map { case fun@FunDef(name, _, _, _) =>  (name, eval(fun)(env)) }

      // Add all functions to the functions environment (recursion)
      funcs foreach { case (_, func@Func(_, _, _)) => func.withVals(funcs) }

      eval(body)(env.withVals(funcs))
    case App(fun, args) =>
      // Evaluate the arguments
      val eargs = args map { arg => eval(arg)(env) }

      // Evaluate the function to be called.
      eval(fun)(env) match {
        case Func(fargs, fbody, fenv) =>
          eval(fbody)(fenv.withVals(fargs zip eargs))
        case Primitive("getchar") => Cst(Console.in.read)
        case Primitive("putchar") =>
          val List(Cst(c: Int)) = eargs
          Console.out.write(c)
          Console.out.flush
          Cst(())
      }
    case ArrayDec(size, _) =>
      val Cst(s: Int) = eval(size)(env)
      Cst(new Array[Any](s))
  }

}

/*
 * Defintion of the value produces by the StackInterpreter
 */
object StackVal extends BugReporter {
  import Language._

  type Loc = Int

  /*
   * Location of primitives operators. Already stored in the environment.
   */
  val primitives = Map[String, Loc](
    "putchar" -> 0,
    "getchar" -> 1
  )

  /**
   * Env of the interpreter. Keep track of the location
   * in memory of each variable defined.
   */
  case class Env(
    vars: Map[String, Loc] = primitives) {

      def undef(name: String) =
        BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")
      /*
       * Return a copy of the current state plus a
       * variable 'name' at the location 'loc'
       */
      def withVal(name: String, loc: Loc): Env = {
        copy(vars = vars + (name -> loc))
      }

      /*
       * Return a copy of the current state plus a
       * variable 'name' at the location 'loc'
       */
      def withVals(list: List[(String,Loc)]): Env = {
        copy(vars = vars ++ list.toMap)
      }

      /*
       * Return the location of the variable 'name'
       */
      def apply(name: String) = vars.get(name) match {
        case Some(loc) => loc
        case None => undef(name)
      }
  }

  abstract class Val

  // Constant values: Int, Boolean, Array
  case class Cst(x: Any) extends Val {
    override def toString = if (x != null) x.toString else "null"
  }

  // Function values
  case class Func(args: List[String], fbody: Tree, var env: Env) extends Val {
    def withVals(list: List[(String,Int)]) = env = env.withVals(list)
    override def toString = s"(${args mkString ","}) => $fbody"
  }

  // Primitives
  case class Primitive(name: String) extends Val
}

/**
 * This interpreter is a stack-based interpreter as we have seen
 * during the lecture.
 *
 * Rather than returning the value of a node, it stores it in memory,
 * following a well-establish convention.
 *
 * This interpreter works in a similar manner as a processor.
 */
object StackInterpreter extends (Tree => Int) with BugReporter {
  import Language._
  import StackVal._

  // Memory and flag used by the interpreter
  val memory = new Array[Val](1000)
  memory(0) = Primitive("putchar")
  memory(1) = Primitive("getchar")
  var flag: Boolean = true

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and store it at 'sp'
   */
  def evalUn(op: String)(sp: Loc) = (op, memory(sp)) match {
    case ("+", _) => ()
    case ("-", Cst(x: Int)) => memory(sp) = Cst(-x)
    case _ => BUG(s"Unary operator $op undefined")
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and 'sp1', and store it at 'sp'
   *
   * TODO: implement the missing case
   */
  def evalBin(op: String)(sp: Loc, sp1: Loc) = (op, memory(sp), memory(sp1)) match {
    case ("+",  Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x + y)
    case ("-",  Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x - y)
    case ("*",  Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x * y)
    case ("/",  Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x / y)
    case ("==", Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x == y)
    case ("!=", Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x != y)
    case ("<=", Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x <= y)
    case (">=", Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x >= y)
    case ("<" , Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x <  y)
    case (">" , Cst(x: Int), Cst(y: Int)) => memory(sp) = Cst(x >  y)
    case ("block-get", Cst(arr: Array[Any]), Cst(i: Int)) => ???
    case _    => BUG(s"Binary operator $op undefined")
  }

  /*
   * TODO: implement the missing case.
   */
  def evalTer(op: String)(sp: Loc, sp1: Loc, sp2: Loc) = (op, memory(sp), memory(sp1), memory(sp2)) match {
    case ("block-set", Cst(arr: Array[Any]), Cst(i: Int), Cst(x)) => ???
    case _ => BUG(s"ternary operator $op undefined")
  }

  def evalPrim(op: String)(idxs: List[Int]) = idxs match {
    case List(sp, sp1, sp2) => evalTer(op)(sp, sp1, sp2)
    case List(sp, sp1)      => evalBin(op)(sp, sp1)
    case List(sp)           => evalUn(op)(sp)
    case _ => BUG(s"no prim with ${idxs.length} arguments")
  }

  /*
   * Evaluate the value of the AST 'tree' within
   * an empty environment and return the value.
   */
  def apply(tree: Tree): Int = {
    // Start at 2, putchar and getchar are store at 0 and 1!!
    eval(tree, 2)(Env())
    val Cst(res: Int) = memory(2)
    res
  }

  /*
   * Evaluate the value of the AST 'tree' within
   * the environment 'env 'and store the result
   * at 'sp'.
   *
   * NOTE: Cond stores its result in the 'flag'
   * variable.
   *
   * TODO: Remove all ???s and implement the
   * appropriate code. The result must be the
   * same than the evaluator defined above.
   */
  def eval(tree: Tree, sp: Loc)(env: Env): Unit = tree match {
    case Lit(x: Unit) => ()
    case Lit(x) =>
      memory(sp) = Cst(x)
    case Prim(op, args) =>
      val idxs = List.tabulate(args.length)(i => sp + i)
      (args zip idxs) foreach { case (arg, idx) => eval(arg, idx)(env) }
      evalPrim(op)(idxs)
    case Let(x, tp, a, b) =>
      eval(a, sp)(env)
      eval(b, sp + 1)(env.withVal(x, sp))
      memory(sp) = memory(sp + 1)
    case Ref(x) =>
      memory(sp) = memory(env(x))
    case If(cond, tBranch, eBranch) =>
      eval(cond, sp)(env)
      val Cst(flag: Boolean) = memory(sp)
      if (flag)
        eval(tBranch, sp)(env)
      else
        eval(eBranch, sp)(env)
    case VarDec(x, tp, rhs, body) =>
      eval(rhs, sp)(env)
      eval(body, sp + 1)(env.withVal(x, sp))
      memory(sp) = memory(sp + 1)
    case VarAssign(x, rhs) =>
      eval(rhs, sp)(env)
      memory(env(x)) = memory(sp)
    case While(cond, lbody, body) =>
      eval(cond, sp)(env)
      while (memory(sp) == Cst(true)) {
        eval(lbody, sp)(env)
        eval(cond, sp)(env)
      }
      eval(body, sp)(env)
    case FunDef(_, args, _, fbody) =>
      ???
    case LetRec(funs, body) =>
      // TODO: modify that code
      eval(body, sp)(env)
    case App(fun, args) =>
      ???
    case ArrayDec(size, _) =>
      ???

  }
}
