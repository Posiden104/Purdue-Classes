package test.miniscala

import java.io._
import org.junit.Test

import miniscala._

class ParserTest {

  import Language._

  def scanner(src: String) = new Scanner(new BaseReader(src, '\u0000'))

  def testGenericPrecedence(op: String, res: Tree) = {
    val gen = new ArithParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }


  //getNum
  @Test def test1 = {
    testGenericPrecedence("1", Lit(1))
  }
  @Test def test2 = {
    testGenericPrecedence("2-4*3", Prim("-", Lit(2), Prim("*", Lit(4), Lit(3))))
  }
}
