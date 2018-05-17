package miniscala

import java.io._
import scala.sys.process._

class AbortException extends Exception("aborted")

// Error reporting
trait Reporter {

  // report an error
  def error(s: String): Unit = System.err.println(s"Error: $s.")
  // report error and halt
  def abort(s: String): Nothing = { error(s); throw new AbortException() }

  def expected(s: String): Nothing = abort(s"$s expected")
}

// Utilities to emit code
abstract class Codegen extends (Tree => String) {

  // Create PrinWriter
  val out = new ByteArrayOutputStream
  val stream = new PrintWriter(out, true)

  def apply(prog: Tree) = {
    emitCode(prog)
    out.toString.stripLineEnd
  }

  def emitCode(prog: Tree): Unit

  // output
  def emit(s: String): Unit = stream.print('\t' + s)
  def emitln(s: String): Unit = stream.println('\t' + s)
}

// Scanner/Parser base classes
abstract class Reader[T] {
  def peek: T
  def hasNext: Boolean
  def hasNext(f: T => Boolean): Boolean
  def next(): T
}

class BaseReader(str: String, eof: Char) extends Reader[Char] {
  val in = str.iterator
  var peek = if (in.hasNext) in.next() else eof
  def hasNext: Boolean = peek != eof
  def hasNext(f: Char => Boolean) = f(peek)
  def next() = {
    val res = peek
    peek = if (in.hasNext) in.next() else eof
    res
  }
}

// ASM bootstrapping
class ASMRunner(snipet: String) {

  val code =
    s"""|.text
        |\t.global entry_point
        |
        |entry_point:
        |\tpush %rbp\t# save stack frame for C convention
        |\tmov %rsp, %rbp
        |
        |\t# beginning generated code
        |${snipet}
        |\t# end generated code
        |\t# %rax contains the result
        |
        |\tmov %rbp, %rsp\t# reset frame
        |\tpop %rbp
        |\tret
        |""".stripMargin

  def assemble = {
    val file = new File("gen/gen.s")
    val writer = new PrintWriter(file)

    writer.println(code)
    writer.flush
    writer.close

    // Launch a process that call gcc
    Seq("gcc","gen/bootstrap.c","gen/gen.s","-o","gen/out").!.toInt
  }

  def run = {
    val stdout = "gen/out".!!
    // output format: Result: <res>\n
    stdout.split(" ").last.trim.toInt
  }
}
