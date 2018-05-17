package miniscala
package test

//import com.sun.org.apache.xpath.internal.operations.Plus
import java.io._
import org.junit.Test

class ParserTest {

  // Function Helper for SingleAddOpParser
  def testSingleAdd(op: String, res: Tree) = {
    val gen = new SingleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  def reader(src: String) = new BaseReader(src, '\u0000')

  // Function Helper for MultipleAddOpParser
  def testMultipleAdd(op: String, res: Tree) = {
    val gen = new MultipleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  // Function Helper for ArithOpParser
  def testArith(op: String, res: Tree) = {
    val gen = new ArithOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  @Test def test1 = {
    testArith("4+0", Plus(Lit(4), Lit(0)))
  }
  @Test def test2 = {
    testArith("2*3", Times(Lit(2), Lit(3)))
  }
  @Test def test3 = {
    testArith("5+6/3", Plus(Lit(5), Div(Lit(6), Lit(3))))
  }
  @Test def test4 = {
    testArith("5*6+3", Plus(Times(Lit(5), Lit(6)), Lit(3)))
  }
  @Test def test5 = {
    testArith("5*6-3", Minus(Times(Lit(5), Lit(6)), Lit(3)))

  }
  @Test def test6 = {
    testArith("0/6-3", Minus(Div(Lit(0), Lit(6)), Lit(3)))

  }
  @Test def test7 = {
    testArith("1*4-2/3", Minus(Times(Lit(1), Lit(4)), Div(Lit(2), Lit(3))))
  }
  @Test def test8 = {
    testArith("3*4*5*6", Times(Times(Times(Lit(3), Lit(4)), Lit(5)), Lit(6)))
  }
  @Test def test9 = {
    testArith("1+4-5+0", Plus(Minus(Plus(Lit(1), Lit(4)), Lit(5)), Lit(0)))
  }
  @Test def test10 = {
    testArith("1+4-5+7*5", Plus(Minus(Plus(Lit(1), Lit(4)), Lit(5)), Times(Lit(7), Lit(5))))
  }


  // Function Helper for ArithParOpParser
  def testArithPar(op: String, res: Tree) = {
    val gen = new ArithParOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  @Test def test11 = {
    testArithPar("-1", Minus(Lit(0), Lit(1)))
  }
  @Test def test12 = {
    testArithPar("1*4-2/3", Minus(Times(Lit(1), Lit(4)), Div(Lit(2), Lit(3))))
  }
  @Test def test13 = {
    testArithPar("5*1", Times(Lit(5), Lit(1)))

  }
  @Test def test14 = {
    testArithPar("6/3", Div(Lit(6), Lit(3)))

  }
  @Test def test15 = {
    testArithPar("4-3-1", Minus(Minus(Lit(4), Lit(3)), Lit(1)))

  }
  @Test def test16 = {
    testArithPar("4-(3-2)", Minus(Lit(4), Minus(Lit(3), Lit(2))))

  }
  @Test def test17 = {
    testArithPar("(9-6)/3", Div(Minus(Lit(9), Lit(6)), Lit(3)))

  }
  @Test def test18 = {
    testArithPar("4-6*4", Minus(Lit(4), Times(Lit(6), Lit(4))))

  }
  @Test def test19 = {
    testArithPar("-(1+3)*(-5-2)", Times(Minus(Lit(0), Plus(Lit(1), Lit(3))), Minus(Minus(Lit(0), Lit(5)), Lit(2))))

  }
  @Test def test20 = {
    testArithPar("(1+3)*(5-2)", Times(Plus(Lit(1), Lit(3)), Minus(Lit(5), Lit(2))))

  }
}
