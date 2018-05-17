package miniscala

import java.io._
import scala.io._

object Runner {
  import Language._

  def main(args: Array[String]): Unit = {

    if (args.length == 0) {
      println("Usage: run PROG [OPTION]")
      println("   or: run FILE [OPTION]")
      println("OPTION: compX86, intStack, intValue (default)")
      return
    }

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
    // TODO: Change this as you finish parsers
    val parser = new ArrayParser(scanner)
    val ast = try {
      parser.parseCode
    } catch {
      case e: AbortException => return
    }

    println("============= AST ================")
    println(ast)
    println("==================================\n")
    val analyzer = new SemanticAnalyzer(parser)
    println("======= Semantic Analyzer ========")
    val (nast, numWarning, numError) = analyzer.run(ast)
    if (numError > 0) {
      println("==================================\n")
      return
    }
    println("=========== Typed AST ============")
    print(nast)
    println(s": ${nast.tp}")
    println("==================================\n")

    // Interpret or compile the code

    if (args.length > 1 && args(1) == "compX86") {
      // Generate the code
      val code = X86Compiler(nast)

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
    val backEnd = if (args.length == 1 || args(1) == "intValue")
      ValueInterpreter
    else
      StackInterpreter

    println("========== Interpreter ===========")
    println(s"Result ${backEnd(nast)}")
    println("==================================\n")
  }
}

}
