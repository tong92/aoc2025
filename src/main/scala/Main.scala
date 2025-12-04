import scala.io.Source
import day4._

@main def hello(): Unit =
  val source = Source.fromResource("day4.txt")
  val lines = source.getLines().toList
  source.close()

  val result = solve2(lines)
  println(s"Final result: $result")
