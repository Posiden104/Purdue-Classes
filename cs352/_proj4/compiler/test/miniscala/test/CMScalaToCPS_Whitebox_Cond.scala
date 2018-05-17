package miniscala.test

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

class CMScalaToCPS_Whitebox_Cond extends CPSHighTest {
  @Test def testCondNestedTrueTrue =
    testCPSHighTreeEquality("if (if (3 == 4) true else true) 1 else 2", """
      |defc v$1(v$2) = { halt(v$2) };
      |defc v$3() = { vall v$4 = 1; v$1(v$4) };
      |defc v$5() = { vall v$6 = 2; v$1(v$6) };
      |vall v$7 = 3;
      |vall v$8 = 4;
      |if (v$7 == v$8) v$3 else v$3""".stripMargin)

  // TODO add more cases
}
