package test.miniscala

import org.junit.Test
import miniscala._

class CompilerTest {
  import Language._

  val dummyPos = Position(0, 0, 0, 0, 0, 0)

  def testCompiler(ast: Tree, res: Int) = {

    val code = X86Compiler(ast)
    val asm = new ASMRunner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

}
