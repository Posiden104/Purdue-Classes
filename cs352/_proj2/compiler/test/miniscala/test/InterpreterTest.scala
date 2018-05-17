package test.miniscala

import org.junit.Test
import miniscala._

class InterpretTest {
  import Language._

  def testInterpreter(ast: Tree, res: Int) = {

    assert(res == StackInterpreter(ast), "Interpreter does not return the correct value")
  }

  @Test def test1 = {
    testInterpreter(Lit(-21), -21)
  }
}
