import day7._

class makeSplitTest extends munit.FunSuite {
  test("...") {
    assertEquals(makeSplit("...", "..."), "...")
    assertEquals(makeSplit("...", ".^."), ".^.")
    assertEquals(makeSplit("...", "^^^"), "^^^")
  }
  test(".|.") {
    val s = ".|."
    assertEquals(makeSplit(s, "..."), ".|.")
    assertEquals(makeSplit(s, ".^."), "|^|")
    assertEquals(makeSplit(s, "^^^"), "^^^")
  }
  test(".||.") {
    val s = ".||."
    assertEquals(makeSplit(s, "...."), ".||.")
    assertEquals(makeSplit(s, ".^.."), "|^|.")
    assertEquals(makeSplit(s, "^^^."), "^^^|")
    assertEquals(makeSplit(s, ".^^^"), "|^^^")
    assertEquals(makeSplit(s, ".^^."), "|^^|")
  }
}

class calDiffTest extends munit.FunSuite {
  test("no change") {
    assertEquals(calDiff("...", "..."), 0)
    assertEquals(calDiff(".|.", ".|."), 0)
    assertEquals(calDiff("^^^", "^^^"), 0)
  }
  test("some change") {
    assertEquals(calDiff("...", ".^."), 0)
    assertEquals(calDiff("...", "^^^"), 0)
    assertEquals(calDiff(".|.", "|^|"), 1)
    assertEquals(calDiff(".||.", "|^|."), 1)
    assertEquals(calDiff(".||.", "^^^."), 0)
    assertEquals(calDiff(".||.", "|^^^"), 1)
  }
}