package miniscala

import java.lang.Math.{ floorDiv, floorMod }
import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MutableMap }
import IO._

/**
 * A tree-based interpreter for the CPS languages.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

abstract class CPSInterpreter[M <: CPSTreeModule](protected val treeModule: M) {
  import treeModule._

  def apply(tree: Tree): Unit =
    eval(tree, emptyEnv)

  def statistics(tree: Tree): Unit =
    ()

  protected sealed trait Value
  protected case class FunV(retC: Name, args: Seq[Name], body: Tree, env: Env)
      extends Value
  protected case class CntV(args: Seq[Name], body: Tree, env: Env)
      extends Value

  protected type Env = PartialFunction[Name, Value]
  protected val emptyEnv: Env = Map.empty

  @tailrec
  private def eval(tree: Tree, env: Env): Unit = {
    statistics(tree: Tree)
    (tree: @unchecked) match {
      case LetL(name, lit, body) =>
        eval(body, Map(name -> evalLit(lit)) orElse env)

      case LetP(name, prim, args, body) =>
        eval(body, Map(name -> evalValuePrim(prim, args map env)) orElse env)

      case LetC(cnts, body) =>
        val recEnv = MutableMap[Name, Value]()
        val env1 = recEnv orElse env
        for (CntDef(name, args, body) <- cnts)
          recEnv(name) = CntV(args, body, env1)
        eval(body, env1)

      case LetF(funs, body) =>
        val recEnv = MutableMap[Name, Value]()
        val env1 = recEnv orElse env
        for (FunDef(name, retC, args, body) <- funs) {
          recEnv(name) = wrapFunV(FunV(retC, args, body, env1))
        }
        eval(body, env1)

      case AppC(cnt, args) =>
        val CntV(cArgs, cBody, cEnv) = env(cnt)
        assume(cArgs.length == args.length)
        eval(cBody, Map(cArgs zip (args map env) : _*) orElse cEnv)

      case AppF(fun, retC, args) =>
        val FunV(fRetC, fArgs, fBody, fEnv) = unwrapFunV(env(fun))
        assume(fArgs.length == args.length)
        eval(fBody,
             Map((fRetC +: fArgs) zip ((retC +: args) map env): _*) orElse fEnv)

      case If(cond, args, thenC, elseC) =>
        val cnt = if (evalTestPrim(cond, args map env)) thenC else elseC
        val cntV = env(cnt).asInstanceOf[CntV]
        eval(cntV.body, cntV.env)

      case Halt(arg) =>
        val x = unwrapIntV(env(arg))
        println(s"\nExit Code: $x")
    }
  }

  protected def unwrapIntV(intv: Value): Int
  protected def wrapFunV(funV: FunV): Value
  protected def unwrapFunV(v: Value): FunV

  protected def evalLit(l: Literal): Value
  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value
  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean
}

object CPSInterpreterHigh extends CPSInterpreterHigh
class CPSInterpreterHigh extends CPSInterpreter(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => Unit) with BugMiniScalaReporter {
  import treeModule._

  private case class BlockV(tag: Int, contents: Array[Value]) extends Value
  private case class IntV_(value: Int) extends Value {
    require(BitTwiddling.fitsInNSignedBits(31)(value))
  }
  private case class CharV(value: Int) extends Value
  private case class BooleanV(value: Boolean) extends Value
  private case object UnitV extends Value

  private object IntV {
    def apply(v: Int): Value = IntV_((v << 1) >> 1)
    def unapply(v: Value): Option[Int] = v match {
      case IntV_(i) => Some(i)
      case _ => None
    }
  }

  protected def unwrapIntV(intV: Value): Int = intV match {
    case IntV(x) => x
    case _ => BUG("return type should be integer")
  }

  protected def wrapFunV(funV: FunV): Value =
    BlockV(BlockTag.Function.id, Array(funV))
  protected def unwrapFunV(v: Value): FunV = v match {
    case BlockV(_, Array(funV: FunV)) => funV
  }

  protected def evalLit(l: Literal): Value = l match {
    case IntLit(i) => IntV(i)
    case CharLit(c) => CharV(c)
    case BooleanLit(b) => BooleanV(b)
    case UnitLit => UnitV
  }

  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value =
    (p, args) match {
      case (MiniScalaBlockAlloc(t), Seq(IntV(i))) => BlockV(t, Array.fill(i)(UnitV))
      case (MiniScalaBlockTag, Seq(BlockV(t, _))) => IntV(t)
      case (MiniScalaBlockLength, Seq(BlockV(_, c))) => IntV(c.length)
      case (MiniScalaBlockGet, Seq(BlockV(_, v), IntV(i))) => v(i)
      case (MiniScalaBlockSet, Seq(BlockV(_, v), IntV(i), o)) => v(i) = o; UnitV

      case (MiniScalaIntAdd, Seq(IntV(v1), IntV(v2))) => IntV(v1 + v2)
      case (MiniScalaIntSub, Seq(IntV(v1), IntV(v2))) => IntV(v1 - v2)
      case (MiniScalaIntSub, Seq(IntV(v1)))           => IntV(-v1)
      case (MiniScalaIntMul, Seq(IntV(v1), IntV(v2))) => IntV(v1 * v2)
      case (MiniScalaIntDiv, Seq(IntV(v1), IntV(v2))) => IntV(floorDiv(v1, v2))
      case (MiniScalaIntMod, Seq(IntV(v1), IntV(v2))) => IntV(floorMod(v1, v2))
      case (MiniScalaIntToChar, Seq(IntV(v))) => CharV(v)

      case (MiniScalaIntArithShiftLeft, Seq(IntV(v1), IntV(v2))) => IntV(v1 << v2)
      case (MiniScalaIntArithShiftRight, Seq(IntV(v1), IntV(v2))) => IntV(v1 >> v2)
      case (MiniScalaIntBitwiseAnd, Seq(IntV(v1), IntV(v2))) => IntV(v1 & v2)
      case (MiniScalaIntBitwiseOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 | v2)
      case (MiniScalaIntBitwiseXOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 ^ v2)

      case (MiniScalaByteRead, Seq()) => IntV(readByte())
      case (MiniScalaByteWrite, Seq(IntV(c))) => writeByte(c); UnitV
      case (MiniScalaCharToInt, Seq(CharV(c))) => IntV(c.toInt)

      case (MiniScalaId, Seq(v)) => v
    }

  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean =
    (p, args) match {
      case (MiniScalaBlockP, Seq(BlockV(_, _))) => true
      case (MiniScalaBlockP, Seq(_)) => false

      case (MiniScalaIntP, Seq(IntV(_))) => true
      case (MiniScalaIntP, Seq(_)) => false
      case (MiniScalaIntLt, Seq(IntV(v1), IntV(v2))) => v1 < v2
      case (MiniScalaIntLe, Seq(IntV(v1), IntV(v2))) => v1 <= v2
      case (MiniScalaIntGe, Seq(IntV(v1), IntV(v2))) => v1 >= v2
      case (MiniScalaIntGt, Seq(IntV(v1), IntV(v2))) => v1 > v2

      case (MiniScalaCharP, Seq(CharV(_))) => true
      case (MiniScalaCharP, Seq(_)) => false

      case (MiniScalaBoolP, Seq(BooleanV(_))) => true
      case (MiniScalaBoolP, Seq(_)) => false

      case (MiniScalaUnitP, Seq(UnitV)) => true
      case (MiniScalaUnitP, Seq(_)) => false

      case (MiniScalaEq, Seq(v1, v2)) => v1 == v2
      case (MiniScalaNe, Seq(v1, v2)) => v1 != v2
    }
}

