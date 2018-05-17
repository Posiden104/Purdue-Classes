package test.miniscala

import org.junit.Test
import miniscala._

class CompilerTest {
  import Language._

  def testCompiler(ast: Tree, res: Int) = {

    val code = X86Compiler(ast)
    val asm = new ASMRunner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  @Test def test1 = {
    testCompiler(Lit(-21), -21)
  }
}
