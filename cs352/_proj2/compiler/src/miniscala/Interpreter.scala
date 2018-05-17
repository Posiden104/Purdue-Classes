package miniscala

import scala.collection.mutable.HashMap
import Language._

/**
 * This interpreter specifies the semantics of our
 * programming lanaguage.
 *
 * The evaluation of each node returns a value.
 */
object ValueInterpreter extends (Tree => Int) with BugReporter {

  type Val = Int

  /**
   * Env of the interpreter. Keeps track of the value
   * of each variable defined.
   */
  case class BoxedVal(var x: Val)
  case class Env(vars: Map[String, BoxedVal] = Map.empty) {
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
     * Update the variable 'name'
     * Return the new value of the variable
     */
    def updateVar(name: String, v: Val): Val = {
      if (vars.contains(name))
        vars(name).x = v
      else
        undef(name)
      v
    }

    /*
     * Return the value of the variable 'name'
     */
    def apply(name: String): Val = {
      if (vars.contains(name))
        vars(name).x
      else
        undef(name)
    }
  }

  /*
   * Compute and return the result of the unary
   * operation 'op' on the value 'v'
   */
  def evalUn(op: String)(v: Val) = op match {
    case "-" => -v
    case "+" => v
  }

  /*
   * Compute and return the result of the binary
   * operation 'op' on the value 'v' and 'w'
   * Note: v op w
   */
  def evalBin(op: String)(v: Val, w: Val) = op match {
    case "-" => v-w
    case "+" => v+w
    case "*" => v*w
    case "/" => v/w
  }

  /*
   * Compute and return the result of the condition
   * operation 'op' on the value 'v' and 'w'
   * Note: v op w
   */
  def evalCond(op: String)(v: Val, w: Val) = op match {
    case "==" => v == w
    case "!=" => v != w
    case "<=" => v <= w
    case ">=" => v >= w
    case "<"  => v < w
    case ">"  => v > w
  }

  /*
   * Evaluate the AST starting with an empty Env
   */
  def apply(exp: Tree) = eval(exp)(Env())

  /*
   * Evaluate the AST within the environment 'env'
   */
  def eval(exp: Tree)(env: Env): Val = exp match {
    case Lit(x) => x
    case Unary(op, v) =>
      evalUn(op)(eval(v)(env))
    case Prim(op, lop, rop) =>
      evalBin(op)(eval(lop)(env), eval(rop)(env))
    case Let(x, a, b) =>
      eval(b)(env.withVal(x, eval(a)(env)))
    case Ref(x) =>
      env(x)
    case If(Cond(op, l, r), tBranch, eBranch) =>
      if (evalCond(op)(eval(l)(env), eval(r)(env)))
        eval(tBranch)(env)
      else
        eval(eBranch)(env)
    case VarDec(x, rhs, body) =>
      eval(body)(env.withVal(x, eval(rhs)(env)))
    case VarAssign(x, rhs) =>
      env.updateVar(x, eval(rhs)(env))
    case While(Cond(op, l, r), lBody, body) =>
      while (evalCond(op)(eval(l)(env), eval(r)(env))) {
        eval(lBody)(env)
      }
      eval(body)(env)
  }

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

  type Val = Int
  type Loc = Int

  /**
   * Env of the interpreter. Keep track of the location
   * in memory of each variable defined.
   */
  case class Env(vars: Map[String, Loc] = Map.empty) {
    def undef(name: String): Loc =
      BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")

    /*
     * Return a copy of the current state plus a
     * variable 'name' at the location 'loc'
     */
    def withVal(name: String, loc: Loc): Env = {
      copy(vars = vars + (name -> loc))
    }

    /*
     * Return the location of the variable 'name'
     */
    def apply(name: String) = vars.get(name) match {
      case Some(loc) => loc
      case None => undef(name)
    }
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and store it at 'sp'
   *
   * TODO: Implement the appropriate code as defined in the handout.
   */
  def evalUn(op: String)(sp: Loc) = op match {
    case _ => BUG(s"Unary operator $op undefined")
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and 'sp1', and store it at 'sp'
   *
   * NOTE: sp op sp1
   */
  def evalBin(op: String)(sp: Loc, sp1: Loc) = op match {
    case "+" => memory(sp) += memory(sp1)
    case "-" => memory(sp) -= memory(sp1)
    case "*" => memory(sp) *= memory(sp1)
    case "/" => memory(sp) /= memory(sp1)
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and 'sp1', and store it in the
   * variable 'flag'.
   *
   * NOTE: sp op sp1
   *
   * TODO: Implement the appropriate code as defined in the handout.
   */
  def evalCond(op: String)(sp: Loc, sp1: Loc) = {
    flag = op match {
      case "==" => memory(sp) == memory(sp1)
      case _    => BUG(s"Binary operator $op undefined")
    }
  }

  // Memory and flag used by the interpreter
  val memory = new Array[Val](1000)
  var flag: Boolean = true

  /*
   * Evaluate the value of the AST 'exp' within
   * an empty environment and return the value.
   */
  def apply(exp: Tree): Val = {
    eval(exp, 0)(Env())
    memory(0)
  }

  /*
   * Evaluate the value of the AST 'exp' within
   * the environment 'env 'and store the result
   * at 'sp'.
   *
   * NOTE: Cond stores its result in the 'flag'
   * variable.
   *
   * TODO: Remove all ???s and implement the
   * appropriate code as defined in the handout.
   */
  def eval(exp: Tree, sp: Loc)(env: Env): Unit = exp match {
    case Lit(x) =>
      memory(sp) = x
    case Unary(op, v) =>
      ???
    case Prim(op, lop, rop) =>
      ???
    case Let(x, a, b) =>
      ???
    case Ref(x) =>
      ???
    case Cond(op, l, r) =>
      ???
    case If(cond, tBranch, eBranch) =>
      ???
    case VarDec(x, rhs, body) =>
      ???
    case VarAssign(x, rhs) =>
      ???
    case While(cond, lbody, body) =>
      eval(cond, sp)(env)
      while (flag) {
        eval(lbody, sp)(env)
        eval(cond, sp)(env)
      }
      eval(body, sp)(env)
  }
}
