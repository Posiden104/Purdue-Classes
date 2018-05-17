package miniscala
package test

import java.io._
import org.junit.Test

class StackGeneratorTest {

  // Function Helper for StackASMGenerator
  def testStackASMGenerator(ast: Tree, res: Int) = {
    val code = StackASMGenerator(ast)
    val asm = new ASMRunner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }


  @Test def test21 =
    testStackASMGenerator(Plus(Lit(2), Lit(1)), 3)

  @Test def test22 = {
    testStackASMGenerator(Plus(Lit(9), Lit(8)), 17)
  }

  @Test def test23 = {
    testStackASMGenerator(Minus(Lit(2), Lit(1)), 1)
  }
  @Test def test24 = {
    testStackASMGenerator(Minus(Lit(7), Lit(9)), -2)
  }
  @Test def test25 = {
    testStackASMGenerator(Times(Lit(2), Lit(3)), 6)
  }
  @Test def test26 = {
    testStackASMGenerator(Times(Lit(2), Lit(-3)), -6)
  }
  @Test def test27 = {
    testStackASMGenerator(Div(Lit(9), Lit(-3)), -3)
  }
  @Test def test28 = {
    testStackASMGenerator(Div(Lit(-9), Lit(3)), -3)
  }
  @Test def test29 = {
    testStackASMGenerator(Times(Plus(Lit(1), Lit(3)), Minus(Lit(2), Lit(4))), -8)
  }
  @Test def test30 = {
    testStackASMGenerator(Minus(Plus(Lit(1), Lit(3)), Div(Lit(2), Lit(4))), 4)
  }
  @Test def test31 = {
    testStackASMGenerator(Plus(Plus(Lit(1), Lit(3)), Times(Lit(2), Lit(4))), 12)
  }
  @Test def test32 = {
    testStackASMGenerator(Times(Lit(-6),Times(Lit(9),Lit(2))), -108)
  }
  @Test def test33 = {
    testStackASMGenerator(Minus(Plus(Lit(1), Lit(9)), Div(Lit(2), Lit(4))), 10)
  }
  @Test def test34 = {
    testStackASMGenerator(Div(Plus(Lit(-1), Lit(3)), Times(Lit(2), Lit(4))), 0)
  }
  @Test def test35 = {
    testStackASMGenerator(Minus(Div(Lit(5), Lit(5)), Times(Lit(2), Lit(4))), -7)
  }
}

class RegGeneratorTest {

  // Function Helper for StackASMGenerator
  def testRegASMGenerator(ast: Tree, res: Int) = {

    val code = RegASMGenerator(ast)
    val asm = new ASMRunner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  @Test def testSingleDigit = {
    testRegASMGenerator(Lit(2), 2)
  }

  @Test def testAdd = {
    testRegASMGenerator(Plus(Lit(2), Lit(1)), 3)
  }

  @Test def testSub = {
    testRegASMGenerator(Minus(Lit(2), Lit(1)), 1)
  }

  @Test def testDiv = {
    testRegASMGenerator(Div(Lit(9), Lit(3)), 3)
    testRegASMGenerator(Div(Lit(-1), Lit(3)), 0)
  }

  @Test def testMul = {
    testRegASMGenerator(Times(Lit(2), Lit(3)), 6)
  }

  @Test def testComplex = {
    testRegASMGenerator(Times(Plus(Lit(1), Lit(3)), Minus(Lit(2), Lit(4))), -8)
    testRegASMGenerator(Plus(Plus(Lit(1), Lit(3)), Times(Lit(2), Lit(4))), 12)
    testRegASMGenerator(Minus(Plus(Lit(1), Lit(3)), Div(Lit(2), Lit(4))), 4)
  }
  @Test def test36 = {
    testRegASMGenerator(Plus(Lit(2), Lit(1)), 3)
  }
  @Test def test37 = {
    testRegASMGenerator(Plus(Lit(9), Lit(8)), 17)
  }

  @Test def test38 = {
    testRegASMGenerator(Minus(Lit(2), Lit(1)), 1)
  }
  @Test def test39 = {
    testRegASMGenerator(Minus(Lit(7), Lit(9)), -2)
  }
  @Test def test40 = {
    testRegASMGenerator(Times(Lit(2), Lit(3)), 6)
  }
  @Test def test41 = {
    testRegASMGenerator(Times(Lit(2), Lit(-3)), -6)
  }
  @Test def test42 = {
    testRegASMGenerator(Div(Lit(9), Lit(-3)), -3)
  }
  @Test def test43 = {
    testRegASMGenerator(Div(Lit(-9), Lit(3)), -3)
  }
  @Test def test44 = {
    testRegASMGenerator(Times(Plus(Lit(1), Lit(3)), Minus(Lit(2), Lit(4))), -8)
  }
  @Test def test45 = {
    testRegASMGenerator(Minus(Plus(Lit(1), Lit(3)), Div(Lit(2), Lit(4))), 4)
  }
  @Test def test46 = {
    testRegASMGenerator(Plus(Plus(Lit(1), Lit(3)), Times(Lit(2), Lit(4))), 12)
  }
  @Test def test47 = {
    testRegASMGenerator(Times(Lit(-6),Times(Lit(9),Lit(2))), -108)
  }
  @Test def test48 = {
    testRegASMGenerator(Minus(Plus(Lit(1), Lit(9)), Div(Lit(2), Lit(4))), 10)
  }
  @Test def test49 = {
    testRegASMGenerator(Div(Plus(Lit(-1), Lit(3)), Times(Lit(2), Lit(4))), 0)
  }
  @Test def test50 = {
    testRegASMGenerator(Minus(Div(Lit(5), Lit(5)), Times(Lit(2), Lit(4))), -7)
  }
}
