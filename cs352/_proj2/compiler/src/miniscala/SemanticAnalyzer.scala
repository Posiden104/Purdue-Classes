package miniscala

class SemanticAnalyzer(parser: Parser) extends Reporter {
  import Language._

  /*
   * Env that keeps track of variables defined.
   * The map stores true if the variable is mutable,
   * false otherwise.
   *
   * NOTE:
   *   val env = new Env
   *
   *   env("hello") is equivalent to env.apply("hello")
   */
  case class Env(vars: Map[String,Boolean] = Map.empty) {

    /*
     * Return true if the variable is already defined
     * in this scope
     */
    def isDefined(name: String) = vars.contains(name)


    /*
     * Make a copy of this object and add a mutable variable 'name'
     */
    def withVar(name: String): Env = {
      copy(vars = vars + (name -> true))
    }

    /*
     * Make a copy of this object and add an immutable variable 'name'
     */
    def withVal(name: String): Env = {
      copy(vars = vars + (name -> false))
    }

    /*
     * Return true if 'name' is a mutable variable defined in this scope
     */
    def isVar(name: String) = vars.get(name) match {
      case None => false
      case Some(mut) => mut
    }

    /*
     * Return true if 'name' is a variable defined in this scope
     */
    def apply(name: String): Boolean = isDefined(name)
  }

  // Error reporting.
  var numError = 0
  def error(msg: String, pos: Position): Unit = {
    numError += 1
    parser.error(msg, pos)
  }

  // Warning reporting.
  var numWarning = 0
  def warn(msg: String, pos: Position): Unit = {
    numWarning += 1
    parser.warn(msg, pos)
  }

  /**
   * Run the Semantic Analyzer on the given AST.
   * Print out the number of warnings and errors
   * found, if any.
   * Return the number of warnings and errors
   */
  def run(exp: Tree) = {
    numError = 0
    analyze(exp)(Env())
    if (numWarning > 0)
      System.err.println(s"""$numWarning warning${if (numWarning != 1) "s" else ""} found""")
    if (numError > 0)
      System.err.println(s"""$numError error${if (numError != 1) "s" else ""} found""")
    else
      System.out.println("Correct semantic")

    (numWarning, numError)
  }

  // List of valid infix operators
  val isOperator   = Set("+","-","*","/")

  // List of valid unary operators
  val isUnOperator   = Set("+","-")

  // List of valid boolean operators
  val isBOperator = Set("==", "!=", "<=", ">=", "<", ">")

  /*
   * Analyze 'exp' with the environment 'env'
   *
   * TODO: Remove the () and add the correct implementation.
   * The code must follow the rules listed in the handout.
   */
  def analyze(exp: Tree)(env: Env): Unit = exp match {
    case Lit(x) =>
      () // Correct there is nothing to check here.
    case Unary(op, v) =>
      if (!isUnOperator(op))
        error("undefined unary operator", exp.pos)
      analyze(v)(env)
    case Prim(op, lop, rop) =>
      ()
    case Let(x, a, b) =>
      if (env.isDefined(x))
        warn("reuse of variable name", exp.pos)
      analyze(a)(env)
      analyze(b)(env.withVal(x))
    case Ref(x) =>
      ()
    case Cond(op, l, r) =>
      ()
    case If(cond, tBranch, eBranch) =>
      analyze(cond)(env)
      analyze(tBranch)(env)
      analyze(eBranch)(env)
    case VarDec(x, rhs, body) =>
      ()
    case VarAssign(x, rhs) =>
      ()
    case While(cond, lBody, body) =>
      ()
    case _ => abort(s"unknown AST node $exp")
  }

}
