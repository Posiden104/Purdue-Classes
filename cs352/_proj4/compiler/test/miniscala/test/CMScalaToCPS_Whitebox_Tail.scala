package miniscala.test

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

class CMScalaToCPS_Whitebox_Tail extends CPSHighTest {
  @Test def testTailUselessContinuations =
    testCPSHighTreeEquality("def f(g: () => Int) = g(); 0", """
      |deff v$1(v$2, v$3) = { v$3(v$2) };
      |vall v$4 = 0;
      |halt(v$4)
      """.stripMargin
    )

  // TODO add more cases
}
