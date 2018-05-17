package miniscala

/**
 * A module for CPS trees.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait CPSTreeModule {
  type Name
  type ValuePrimitive
  type TestPrimitive
  type Literal

  sealed trait Tree {
    /**
     * Produce a new version of the tree where all names have been
     * substituted according to the given (partial) substitution.
     */
    def subst(s: Substitution[Name]): Tree = {
      def substIn(t: Tree): Tree = t match {
        case LetL(name, value, body) =>
          LetL(s(name), value, substIn(body))
        case LetP(name, prim, args, body) =>
          LetP(s(name), prim, args map s, substIn(body))
        case LetC(cnts, body) =>
          val substCnts = cnts map {
            case CntDef(name, args, body) =>
              CntDef(s(name), args map s, substIn(body))
          }
          LetC(substCnts, substIn(body))
        case LetF(funs, body) =>
          val substFuns = funs map {
            case FunDef(name, retC, args, body) =>
              FunDef(s(name), s(retC), args map s, substIn(body))
          }
          LetF(substFuns, substIn(body))
        case AppC(cont, args) =>
          AppC(s(cont), args map s)
        case AppF(fun, retC, args) =>
          AppF(s(fun), s(retC), args map s)
        case If(cond, args, thenC, elseC) =>
          If(cond, args map s, s(thenC), s(elseC))
        case Halt(arg) =>
          Halt(s(arg))
      }

      substIn(this)
    }
  }

  case class LetL(name: Name, value: Literal, body: Tree) extends Tree
  case class LetP(name: Name, prim: ValuePrimitive, args: Seq[Name], body:Tree)
       extends Tree
  case class LetC(cnts: Seq[CntDef], body: Tree) extends Tree
  case class LetF(funs: Seq[FunDef], body: Tree) extends Tree
  case class AppC(cnt: Name, args: Seq[Name]) extends Tree
  case class AppF(fun: Name, retC: Name, args: Seq[Name]) extends Tree
  case class If(cond: TestPrimitive, args: Seq[Name], thenC: Name, elseC: Name)
       extends Tree
  case class Halt(arg: Name) extends Tree

  case class CntDef(name: Name, args: Seq[Name], body: Tree)
  case class FunDef(name: Name, retC: Name, args: Seq[Name], body: Tree)
}

/**
 * Module for "high-level" CPS trees: the full CMScala literals and
 * primitives are available.
 */
object SymbolicCPSTreeModule extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = MiniScalaValuePrimitive
  type TestPrimitive = MiniScalaTestPrimitive
  type Literal = CMScalaLiteral
}

