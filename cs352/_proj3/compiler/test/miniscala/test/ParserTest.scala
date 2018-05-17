package test.miniscala

import org.junit.Test

import miniscala._

class ParserTest {
  import Language._

  val dummyPos = Position(0, 0, 0, 0, 0, 0)

  def rename(exp: Tree): Tree = {
    var next = 0
    def fresh() = { next += 1; s"x$$$next" }

    def subst(name: String)(implicit ctx: Map[String, String]) = ctx.get(name) match {
      case Some(name) => name
      case _ => name
    }

    def renameType(tp: Type): Type = tp match {
      case FunType(args, rte) => FunType(args.map(x => ("", renameType(x._2))), renameType(rte))
      case ArrayType(tp) => ArrayType(renameType(tp))
      case _ => tp
    }

    def renameTree(exp: Tree)(implicit ctx: Map[String,String]): Tree = exp match  {
      case Lit(x) => exp
      case Prim(op, args) =>
        Prim(op, args map(renameTree))
      case Let(x, tp, a, b) =>
        val nx = fresh()
        Let(nx, renameType(tp), renameTree(a), renameTree(b)(ctx + (x -> nx)))
      case Ref(x) =>
        Ref(subst(x))
      case If(cond, tBranch, eBranch) =>
        If(renameTree(cond), renameTree(tBranch), renameTree(eBranch))
      case VarDec(x, tp, rhs, body) =>
        val nx = fresh()
        VarDec(nx, tp, renameTree(rhs), renameTree(body)(ctx + (x -> nx)))
      case VarAssign(x, rhs) =>
        VarAssign(subst(x), renameTree(rhs))
      case While(cond, lBody, body) =>
        While(renameTree(cond), renameTree(lBody), renameTree(body))
      case FunDef(f, args, rtp, fbody) =>
        val (nargs, nnames) = args.map {x =>
          val nx = fresh()
          (Arg(nx, renameType(x.tp), dummyPos), x.name -> nx)
        }.unzip
        FunDef(subst(f), nargs, renameType(rtp), renameTree(fbody)(ctx ++ nnames.toMap))
      case LetRec(funs, body) =>
        val nfuns = funs.sortWith {
          case (x: FunDef, y: FunDef) => x.name < y.name
        }
        val nctx = ctx ++ (nfuns map { case FunDef(name, _, _, _) =>
          name -> fresh()
        }).toMap
        LetRec(nfuns map(renameTree(_)(nctx)), renameTree(body)(nctx))
      case App(fun, args) =>
        App(renameTree(fun), args map(renameTree(_)))
      case ArrayDec(size, xtp) =>
        ArrayDec(renameTree(size), renameType(xtp))
    }

    renameTree(exp)(Map.empty)
  }

  def scanner(src: String) = new Scanner(new BaseReader(src, '\u0000'))

  def testBaseParser(op: String, res: Tree) = {
    val gen = new BaseParser(scanner(op))
    val ast = gen.parseCode

    assert(rename(ast) == rename(LetRec(Nil, res)), "Invalid result")
  }
  def testSyntacticSugarParser(op: String, res: Tree) = {
    val gen = new SyntacticSugarParser(scanner(op))
    val ast = gen.parseCode

    assert(rename(ast) == rename(LetRec(Nil, res)), "Invalid result")
  }
  def testFunctionParser(op: String, res: Tree) = {
    val gen = new FunctionParser(scanner(op))
    val ast = gen.parseCode

    assert(rename(ast) == rename(res), "Invalid result")
  }
  def testArrayParser(op: String, res: Tree) = {
    val gen = new ArrayParser(scanner(op))
    val ast = gen.parseCode

    assert(rename(ast) == rename(res), "Invalid result")
  }

}
