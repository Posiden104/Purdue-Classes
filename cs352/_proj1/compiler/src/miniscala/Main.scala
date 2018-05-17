package miniscala

import java.io._

object Runner {

  def main(args: Array[String]): Unit = {

    val reader = new BaseReader(args(0), '\u0000')

    // Parser to test!
    val parser = new ArithParOpParser(reader)

    val ast = try {
      parser.parseCode
    } catch {
      case e: AbortException => return
    }

    // print the AST parsed
    println("============= AST ================")
    println(s"\t$ast")
    println("==================================\n")


    val backEnd = (
      RegASMGenerator
      //StackASMGenerator
    )

    // Generate the code
    val code = backEnd(ast)

    val runner = new ASMRunner(code)

    println("============ OUTPUT ==============")
    println(runner.code)
    println("==================================")

    if (runner.assemble != 0) {
      println("Compilation error!")
    } else {
      println(s"Result: ${runner.run}")
    }
  }
}
