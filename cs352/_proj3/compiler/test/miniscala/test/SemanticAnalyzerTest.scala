package test.miniscala

import org.junit.Test
import miniscala._

class SemanticAnalyzerTest {
  import Language._

  def astTypeEquals(ast: Tree, tsa: Tree): Boolean = ast == tsa && ast.tp == tsa.tp && { (ast, tsa) match {
    case (Prim(_, args), Prim(_, sgra))=>
      (args zip sgra) forall { case (arg, gra) => astTypeEquals(arg, gra) }
    case (Let(_, _, a, b), Let(_, _, c, d)) =>
      astTypeEquals(a, c) && astTypeEquals(b, d)
    case (If(cond, tBranch, eBranch), If(cond1, tBranch1, eBranch1)) =>
      astTypeEquals(cond, cond1) && astTypeEquals(tBranch, tBranch1) && astTypeEquals(eBranch, eBranch1)
    case (VarDec(_, _, a, b), VarDec(_, _, c, d)) =>
      astTypeEquals(a, c) && astTypeEquals(b, d)
    case (VarAssign(_, rhs), VarAssign(_, shr)) =>
      astTypeEquals(rhs, shr)
    case (While(cond, tBranch, eBranch), While(cond1, tBranch1, eBranch1)) =>
      astTypeEquals(cond, cond1) && astTypeEquals(tBranch, tBranch1) && astTypeEquals(eBranch, eBranch1)
    case (FunDef(_, _, _, fbody), FunDef(_, _, _, fbody1)) =>
      astTypeEquals(fbody, fbody1)
    case (LetRec(funs, body), LetRec(funs1, body1)) =>
      ((funs zip funs1) forall { case (arg, gra) => astTypeEquals(arg, gra) }) && astTypeEquals(body, body1)
    case (App(fun, args), App(fun1, args1)) =>
      ((args zip args1) forall { case (arg, gra) => astTypeEquals(arg, gra) }) && astTypeEquals(fun, fun1)
    case (ArrayDec(size, _), ArrayDec(size1, _)) =>
      astTypeEquals(size, size1)
    case _ => true
  }}

  def testSemanticAnalyzer(ast: Tree, tsa: Tree, nWarning: Int, nError: Int) = {
    val fakeParser = new Parser(null) {
      override def error(msg: String, pos: Position) = {}
      override def warn(msg: String, pos: Position) = {}
    }

    val analyzer = new SemanticAnalyzer(fakeParser)

    val (tast, w, e) = analyzer.run(ast)
    assert(w == nWarning, "Incorrect number of Warnings")
    assert(e == nError, "Incorrect number of Errors")
    assert(astTypeEquals(tast, tsa), "AST does not have correct type")
  }

}
