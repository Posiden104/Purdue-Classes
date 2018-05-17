package miniscala

class SemanticAnalyzer(parser: Parser) extends Reporter with BugReporter {
  import Language._

  /*
   * Primitive functions that do not need to be defined or declared.
   */
  val primitives = Map[String,(Boolean,Type)](
      "getchar" -> (false, FunType(List(), IntType)),
      "putchar" -> (false, FunType(List(("", IntType)), UnitType))
    )

  /*
   * Define an empty state for the Semantic Analyzer.
   *
   * NOTE:
   *   val env = new Env
   *
   *   env("hello") is equivalent to env.apply("hello")
   */
  case class Env(
    vars: Map[String,(Boolean, Type)] = Map.empty
    ) {

    /*
     * Return true if the variable is already defined
     * in this scope
     */
    def isDefined(name: String) = vars.contains(name)

    /*
     * Make a copy of this object and add a mutable variable 'name'
     */
    def withVar(name: String, tp: Type): Env = {
      copy(vars = vars + (name -> (true, tp)))
    }

    /*
     * Make a copy of this object and add an immutable variable 'name'
     */
    def withVal(name: String, tp: Type): Env = {
      copy(vars = vars + (name -> (false, tp)))
    }

    /*
     * Make a copy of this object and add in the list of immutable variables.
     */
    def withVals(list: List[(String,Type)]): Env = {
      copy(vars = vars ++ (list map { t => (t._1, (false, t._2)) }).toMap)
    }

    /*
     * Return true if 'name' is a mutable variable defined in this scope
     * or in the outer scope.
     */
    def isVar(name: String) = vars.get(name) match {
      case None => false
      case Some((mut, _)) => mut
    }

    /*
     * Return the Type if the variable 'name' is an option.
     * i.e. Some(tp) if the variable exists or None if it doesn't
     */
    def apply(name: String): Option[Type] = vars.get(name) match {
      case Some((_, tp)) => Some(tp)
      case None => None
    }
  }

  // Error reporting
  var numError = 0
  def error(msg: String, pos: Position): Unit = {
    numError += 1
    parser.error(msg, pos)
  }

  // Warning reporting
  var numWarning = 0
  def warn(msg: String, pos: Position): Unit = {
    numWarning += 1
    parser.warn(msg, pos)
  }

  /*
   * Return a fresh name if a new variable needs to be defined
   */
  var next = 0
  def freshName(pref: String = "x") = {
    next += 1
    s"${pref}_$next"
  }

  /*
   * Auxiliary functions. May be useful.
   */
  def getName(arg: Any): String = arg match {
    case Arg(name, _, _) => name
    case FunDef(name, _, _, _) =>  name
    case _ => BUG(s"Don't know how to extract name from $arg")
  }

  def getPos(arg: Any): Position = arg match {
    case Arg(_, _, pos) => pos
    case fd@FunDef(_, _, _, _) => fd.pos
    case _ => BUG(s"Don't know how to extract position from $arg")
  }

  def checkDuplicateNames(args: List[Any]): Boolean = args match {
    case h::t =>
      val name = getName(h)
      val (dup, other) = t partition { arg => name == getName(arg) }
      dup foreach { arg =>
        error(s"$name is already defined", getPos(arg))
      }
      checkDuplicateNames(other) || dup.length > 0
    case Nil => false
  }

  def funType(args: List[Arg], rtp: Type): FunType = {
    FunType(args map { arg => (arg.name, arg.tp) }, rtp)
  }

  def listArgType(size: Int, tp: Type) = List.fill(size)(("", tp))
  def listArgType(tp: Type*) = tp.toList.map(("", _))

  /**
   * Run the Semantic Analyzer on the given AST.
   *
   * Print out the number of warnings and errors found, if any.
   * Return the AST with types resolved and the number of warnings
   * and errors.
   *
   * NOTE: we want our main program to return an Int!
   */
  def run(tree: Tree) = {
    numError = 0
    numWarning = 0
    val ntree = typeCheck(tree, IntType)(Env())
    if (numWarning > 0)
      System.err.println(s"""$numWarning warning${if (numWarning != 1) "s" else ""} found""")
    if (numError > 0)
      System.err.println(s"""$numError error${if (numError != 1) "s" else ""} found""")

    (ntree, numWarning, numError)
  }

  // List of valid infix operators
  val isBOperator   = Set("==","!=","<=",">=","<",">")
  val isIntOperator   = Set("+","-","*","/")

  /*
   * Returns the type of the binary operator 'op'. See case "+" for an example
   * TODO: implement the remaining binary operators for typeBinOperator
   */
  def typeBinOperator(op: String)(pos: Position) = op match {
    case "+" => FunType(listArgType(2, IntType), IntType)
    case "-" => FunType(listArgType(2, IntType), IntType)
    case "*" => FunType(listArgType(2, IntType), IntType)
    case "/" => FunType(listArgType(2, IntType), IntType)
    case _ =>
      error("undefined binary operator", pos)
      UnknownType
  }

  // List of valid unary operators
  val isIntUnOperator   = Set("+","-")

  /*
   * Returns the type of the unary operator 'op'
   * TODO: implement typeUnOperator
   */
  def typeUnOperator(op: String)(pos: Position) = op match {
    case "+" => FunType(listArgType(2, UnitType), UnitType)
    case "-" => FunType(listArgType(2, UnitType), UnitType)
    case _ =>
      error(s"undefined unary operator", pos)
      UnknownType
  }

  /*
   * Returns the type of the ternary operator 'op'
   * TODO: implement typeTerOperator
   * operators: block-set we only consider Integer arrays
   */
  def typeTerOperator(op: String)(pos: Position) = op match {
    //case "block-set" =>
    case _ =>
      error(s"undefined ternary operator", pos)
      UnknownType
  }
  /*
   * Return the type of the operator 'op' with arity 'arity'
   */
  def typeOperator(op: String, arity: Int)(pos: Position): Type = arity match {
    case 3 => typeTerOperator(op)(pos)
    case 2 => typeBinOperator(op)(pos)
    case 1 => typeUnOperator(op)(pos)
    case _ =>
      error(s"undefined operator", pos)
      UnknownType
  }

  /*
   * Check if 'tp' conforms to 'pt' and return the more precise type.
   * The result needs to be well formed.
   *
   * TODO: implement the case of function type.
   */
  def typeConforms(tp: Type, pt: Type)(env: Env, pos: Position): Type = (tp, pt) match {
    case (_, _) if tp == pt => typeWellFormed(tp)(env, pos)
    case (_, UnknownType) => typeWellFormed(tp)(env, pos)  // tp <: Any
    case (UnknownType, _) => typeWellFormed(pt)(env, pos)  // for function arguments
    case (FunType(args1, rtp1), FunType(args2, rtp2)) if args1.length == args2.length => ??? // TODO: Function type conformity has been defined in Lecture 5.
    case (ArrayType(tp), ArrayType(pt)) => ArrayType(typeConforms(tp, pt)(env, pos))
    case (ArrayType(pt), FunType(List((_, tp)), rte)) =>
      typeConforms(tp, IntType)(env, pos)
      ArrayType(typeConforms(pt, rte)(env, pos))
    case _ => error(s"type mismatch;\nfound   : $tp\nexpected: $pt", pos); pt
  }

  /*
   * Auxiliary function used to check function type argument conformity.
   *
   * The function is verifying that 'tp' elements number n conforms
   * to 'pt' element number n. It returns the list of precise types
   * returned by each invocation to typeConforms
   */
  def typeConform(tp: List[(String, Type)], pt: List[(String,Type)])(env: Env, pos: Position): List[(String, Type)] = {
    if (tp.length != pt.length) BUG("length of list does not match")
    (tp zip pt) map { case ((arg1, tp1), (arg2, tp2)) =>
      (if (tp1 != UnknownType) arg1 else arg2, typeConforms(tp1, tp2)(env, pos))
    }
  }

  /*
   * Verify that the type 'tp' is well formed. i.e there is no
   * UnknownType.
   */
  def typeWellFormed(tp: Type)(env: Env, pos: Position): Type = tp match {
    case FunType(args, rte) =>
      FunType(args map { case (n, tp) => (n, typeWellFormed(tp)(env, pos)) }, typeWellFormed(rte)(env, pos))
    case ArrayType(tp) => ArrayType(typeWellFormed(tp)(env, pos))
    case UnknownType => error("malformed type", pos); UnknownType
    case _ => tp
  }

  /*
   * typeCheck takes an expression and an expected type (which may be UnknownType).
   * This is done via calling the typeInfer and typeConforms
   * functions (details below), and finally returning the original
   * expression with all typing information resolved.
   *
   * typeInfer uses the inference rules seen during the lectures
   * to discover the type of an expression. As a reminder, the rules we saw can be
   * found in lectures 5 and 6.
   *
   * TODO: Remove the ??? and add the correct implementation.
   * The code must follow the inference rules seen during the lectures.
   *
   * The errors/warnings check that you had to implement for project 2
   * should be already implemented. However, there are new variables
   * introduced that need to be check for duplicate (function name,
   * variables names). We defined the rules for function semantic in
   * lecture 5.
   */
  def typeCheck(tree: Tree, pt: Type)(env: Env): Tree = {
    val ntree = typeInfer(tree, pt)(env)
    val rntreeType = typeConforms(ntree.tp, pt)(env, tree.pos)
    ntree.withType(rntreeType)
  }

  def typeInfer(tree: Tree, pt: Type)(env: Env): Tree = tree match {
    case Lit(_: Int) => tree.withType(IntType)
    case Lit(_: Boolean) => tree.withType(BooleanType)
    case Lit(_: Unit) => tree.withType(UnitType)
    case Prim(op, args) =>
      typeOperator(op, args.length)(tree.pos) match {
        case FunType(atps, rtp) => ???
        case UnknownType => tree.withType(pt)
        case _ => BUG("operator's type needs to be FunType")
      }
    case Let(x, tp, rhs, body) =>
      if (env.isDefined(x))
        warn("reuse of variable name", tree.pos)
      val nrhs = typeCheck(rhs, tp)(env)
      val nbody = typeCheck(body, pt)(env.withVal(x, nrhs.tp))
      Let(x, nrhs.tp, nrhs, nbody).withType(nbody.tp)
    case Ref(x) =>
      env(x) match {
        case Some(tp) => ???
        case _ =>
          error("undefined identifier", tree.pos)
          tree.withType(pt)
      }
    case If(cond, tBranch, eBranch) =>
      // Hint: type check the else branch before the
      // then branch.
      ???
    case VarDec(x, tp, rhs, body) =>
      if (env.isDefined(x))
        warn("reuse of variable name", tree.pos)
      ???
    case VarAssign(x, rhs) =>
      val xtp = if (!env.isDefined(x)) {
        error("undefined identifier", tree.pos)
        pt
      } else {
        if (!env.isVar(x))
          error("reassignment to val", tree.pos)
        env(x).get
      }

      val nrhs: Tree = ???
      /*
       * Because of syntactic sugar, the variable
       * assignment can be accepted for an expression
       * of type Unit. In this case, we will modify
       * the AST and store the assignment value into
       * a "dummy" variable an return the Unit Literal.
       *
       * For example,
       *
       * If(..., VarAssign("x", Lit(1)), Lit(()))
       *
       * requires the two branches of the If to be of the same
       * type. In this case Unit. Therefore the then branch
       * will need to be modify to have the correct type.
       * Without changing the semantic!
       */
      if (pt == UnitType && nrhs.tp != UnitType) {
        ???
      } else {
        ???
      }
    case While(cond, lbody, body) =>
      ???
    case FunDef(fname, args, rtp, fbody) =>
      ???
    case LetRec(funs, body) =>
      // TODO modify to handle general case
      val nbody = typeCheck(body, pt)(env)
      LetRec(Nil, nbody).withType(nbody.tp)
    case App(fun, args) =>
      // Check fun type
      val nFun: Tree = ???

      // Handling some errors
      val ftp = nFun.tp match {
        case ftp@FunType(fargs, _) if fargs.length == args.length => ftp
        case ftp@FunType(fargs, rtp) if fargs.length < args.length =>
          error(s"too many arguments for method: ($fargs)$rtp", tree.pos)
          FunType(fargs ++ List.fill(args.length - fargs.length)(("", UnknownType)), rtp)
        case ftp@FunType(fargs, rtp) =>
          error(s"not enough arguments for method of type: $ftp", tree.pos)
          ftp
        case ArrayType(tp) =>
          FunType(List(("", IntType)), tp)
        case tp =>
          error(s"$tp does not take paramters", tree.pos)
          FunType(List.fill(args.length)(("", UnknownType)), pt)
      }

      // Check arguments type
      val nargs: List[Tree] = ???

      // Transform some function applications into primitives on arrays.
      nFun.tp match {
        case ArrayType(tp) =>
          Prim("block-get", List(nFun, nargs.head)).withType(tp)
        case _ => App(nFun, nargs).withType(ftp.rtp)
      }
    case ArrayDec(size: Tree, etp: Type) =>
      ???
    case _ => BUG(s"malformed tree $tree")
  }
}
