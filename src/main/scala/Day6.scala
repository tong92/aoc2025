package day6

import scala.util.matching.Regex

trait Op:
    def calc(other: Long): Op
    def result: Long
object Op:
    def apply(v: String): Op =
        v match
            case "+" => Add(0)
            case "*" => Multiply(1)
            case _ => throw Exception(s"Unknown operation: $v")

case object No extends Op:
    def calc(other: Long): Op = this
    def result: Long = 0
case class Add(value: Long) extends Op:
    def calc(other: Long): Op = Add(value + other)
    def result: Long = value
case class Multiply(value: Long) extends Op:
    def calc(other: Long): Op = Multiply(value * other)
    def result: Long = value


def solve1(lines: List[String]): Long =
    lines.map(_.split(" ").toList)
        .reverse
        .foldLeft(Nil):
            (acc, v) =>
                acc match
                    case Nil => 
                        v.filterNot(_.isEmpty).map(Op(_))
                    case _ =>
                        acc.zip(v.filterNot(_.isEmpty)).map:
                            (op, str) => op.calc(str.toLong)
        .map(_.result)
        .sum

def takeOpWithLength(v: List[Char], op: Op, start: Int, cnt: Int): List[(Op, Int, Int)] =
    v match
        case ' ' :: tail => takeOpWithLength(tail, op, start, cnt + 1)
        case '+' :: tail => (op, start, cnt - 1) :: takeOpWithLength(tail, Add(0), start + cnt, 1)
        case '*' :: tail => (op, start, cnt - 1) :: takeOpWithLength(tail, Multiply(1), start + cnt, 1)
        case _ => (op, start, cnt) :: Nil
    
def solve2(lines: List[String]): Long =
    val ops = lines.takeRight(1).map:
            v =>takeOpWithLength(v.toList, No, 0, 0)
        .flatten
    val vs = lines.dropRight(1)
    ops.drop(1).map: (op, start, cnt) =>
        vs.map:
                v => v.substring(start, start + cnt).split("")
            .reduceLeft: (a, b) =>
                a.zip(b).map(_ + _)
            .map:
               _.trim.toLong
            .foldLeft(op):
                (acc, n) => acc.calc(n)
        .result
    .sum
    