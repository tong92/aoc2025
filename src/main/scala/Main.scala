import scala.io.Source
import day8._

@main def hello(): Unit =
  // val source = Source.fromResource("day8.txt")
  val source = Source.fromResource("day8.sample.txt")
  val lines = source.getLines().toList
  source.close()

  val result = solve1(lines)
  // val result = solve2(lines)
  println(s"Final result: $result")
