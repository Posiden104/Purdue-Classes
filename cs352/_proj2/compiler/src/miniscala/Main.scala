package miniscala

import java.io._
import scala.io._

object Runner {

  def main(args: Array[String]): Unit = {

    if (args.length == 0) {
      println("Usage: run PROG [OPTION]")
      println("   or: run FILE [OPTION]")
      println("OPTION: compStack, compX86, intStack, intValue (default)")
      return
    }

    // If input is a valid file name, read the file
    val src = if (new File(args(0)).exists) {
      val source = Source.fromFile(args(0))
      try source.getLines mkString "\n" finally source.close()
    } else {
      args(0)
    }

    println("============ SRC CODE ============")
    println(src)
    println("==================================\n")

    val reader = new BaseReader(src, '\u0000')
    val scanner = new Scanner(reader)

    // Parser to test!
    // // TODO: Change this as you finish parsers
    val parser = new LoopParser(scanner)

    val ast = try {
      parser.parseCode
    } catch {
      case e: AbortException => return
    }

    // print the AST parsed
    println("============= AST ================")
    println(s"\t$ast")
    println("==================================\n")

    val analyzer = new SemanticAnalyzer(parser)
    println("======= Semantic Analyzer ========")
    val (numWarning, numError) = analyzer.run(ast)
    println("==================================\n")
    if (numError > 0)
        return

    // Interpret or compile the code

    if (args.length > 1 && args(1).startsWith("comp")) {

      if (args(1) == "compX86") {
        // Generate the code
        val code = X86Compiler(ast)

        val runner = new ASMRunner(code)

        println("============ OUTPUT ==============")
        println(runner.code)
        println("==================================")

        if (runner.assemble != 0) {
          println("Compilation error!")
        } else {
          println(s"Result: ${runner.run}")
        }
      } else {
        val code = StackCompiler(ast)

        println("============ OUTPUT ==============")
        println(code)
        println("==================================")
      }

    } else {
      val backEnd = if (args.length == 1 || args(1) == "intValue")
        ValueInterpreter
      else
        StackInterpreter

      println("========== Interpreter ===========")
      println(s"Result ${backEnd(ast)}")
      println("==================================\n")

    }
  }
}
