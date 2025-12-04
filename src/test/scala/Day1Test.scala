import day1._
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Solve2Test extends munit.FunSuite {
  test("0 to L10") {
    assertEquals(solve2(List("L10"), 0, 0), 0)
  }
  test("0 to L1000") {
    assertEquals(solve2(List("L1000"), 0, 0), 10)
  }
  test("10 to L1000") {
    assertEquals(solve2(List("L1000"), 10, 0), 10)
  }
  test("10 to L1010") {
    assertEquals(solve2(List("L1010"), 10, 0), 11)
  }
  test("90 to L1000") {
    assertEquals(solve2(List("L1000"), 90, 0), 10)
  }
  test("90 to L1010") {
    assertEquals(solve2(List("L1010"), 90, 0), 10)
  }
  test("90 to L1090") {
    assertEquals(solve2(List("L1090"), 90, 0), 11)
  }
  test("90 to R100") {
    assertEquals(solve2(List("R100"), 90, 0), 1)
  }
  test("90 to R110") {
    assertEquals(solve2(List("R110"), 90, 0), 2)
  }
  test("0 to R100") {
    assertEquals(solve2(List("R100"), 0, 0), 1)
  }
  test("0 to R150") {
    assertEquals(solve2(List("R150"), 0, 0), 1)
  }
  test("RL") {
    assertEquals(solve2(List("R100", "L150"), 50, 0), 3)
  }
  test("LLR") {
    assertEquals(solve2(List("L100", "L150", "R70"), 50, 0), 3)
  }
  test("LLRR") {
    assertEquals(solve2(List("L100", "L150", "R70", "R70"), 50, 0), 4)
  }
  test("sample") {
    assertEquals(solve2(List("L68",
    "L30",
"R48",
"L5",
"R60",
"L55",
"L1",
"L99",
"R14",
"L82"), 50, 0), 6)
  }
}