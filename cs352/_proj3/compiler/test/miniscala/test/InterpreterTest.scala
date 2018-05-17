package miniscala.test

import org.junit.Test
import miniscala._

class InterpreterTest {
  import Language._

  val dummyPos = Position(0, 0, 0, 0, 0, 0)

  def testInterpreter(ast: Tree, res: Int) = {

    assert(res == StackInterpreter(ast), "Interpreter does not return the correct value")
  }

}
