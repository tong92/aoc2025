import day4._

class countRollTest extends munit.FunSuite {
  test(".@.") {
    assertEquals(countRoll(".@.", 0), 1)
    assertEquals(countRoll(".@.", 1), 1)
    assertEquals(countRoll(".@.", 2), 1)
  }
  test(".@@.") {
    val x = ".@@."
    assertEquals(countRoll(x, 0), 1)
    assertEquals(countRoll(x, 1), 2)
    assertEquals(countRoll(x, 2), 2)
    assertEquals(countRoll(x, 3), 1)
  }
}