package day7

def solve1(input: List[String]): Int =
    calSplit(input.head.replaceAllLiterally("S", "|"), input.tail, 0)

def calSplit(input: String, next: List[String], acc: Int): Int =
  next match
    case Nil => acc
    case head :: tail =>
      val res = makeSplit(input, head)
      val value = calDiff(input, res)
      calSplit(res, tail, acc + value)

def makeSplit(input: String, instruction: String): String =
    input.sliding(2).zip(instruction.sliding(2)).map(
        (o, n) =>
            (o, n) match
                case ("..", _) => n
                case ("|.", "^.") => "^|"
                case ("||", "^.") => "^|"
                case (".|", ".^") => "|^"
                case ("||", ".^") => "|^"
                case (_, "^^") => "^^"
                case (_, "..") => o.replaceAllLiterally("^", ".")
                case (_, _) => o
    ).reduce: (a, b) =>
        if a.last == '^' then a + b.tail
        if a.last == '.' then a.dropRight(1) + b
        else if b.head == '^' then a.dropRight(1) + b
        else a + b.tail

def calDiff(oldStr: String, newStr: String): Int =
    oldStr.sliding(3).zip(newStr.sliding(3)).count: (o, n) =>
        (o, n) match
            case ("|||", "|^|") => true
            case (".|.", "|^|") => true
            case ("||.", "|^|") => true
            case (".||", "|^|") => true
            case ("|||", "|^^") => true
            case (".||", "|^^") => true
            case (".|.", "|^^") => true
            case ("||.", "|^^") => true
            case ("|||", "^^|") => true
            case (".||", "^^|") => true
            case (".|.", "^^|") => true
            case ("||.", "^^|") => true
            case _ => false

def solve2(input: List[String]): Long =
    val result = calSplit2(input.head.replaceAllLiterally("S", "|"), input.tail)
        .zipWithIndex
        .filter(_._2 % 2 == 0)
        .map: str =>
            str._1.zipWithIndex.filter(_._1 == '|').map(_._2).toList
    calLeftRight(result.tail, result.head.map((_, 1L)).toMap)

def calSplit2(input: String, next: List[String]): List[String] =
    next match
        case Nil => input :: Nil
        case head :: tail =>
            val res = makeSplit(input, head)
            input :: calSplit2(res, tail)

def calLeftRight(positions: List[List[Int]], cur: Map[Int, Long]): Long =
    positions match
        case Nil => cur.values.sum
        case head :: tail =>
            // check straight and split
            val (i, n) = cur.partition(pos => head.contains(pos._1))
            val next = combine(i, combine(left(head, n), right(head, n)))
            calLeftRight(tail, next)

def left = (next, cur) => getNext(next, cur, -1)

def right = (next, cur) => getNext(next, cur, 1)

def getNext(next: List[Int], cur: Map[Int, Long], i: Int): Map[Int, Long] =
    cur.filter((k, _) => next.contains(k + i))
        .map((k, v) => (k + i, v))

def combine(a: Map[Int, Long], b: Map[Int, Long]): Map[Int, Long] =
    a.foldLeft(b): (acc, o) =>
        acc.updated(o._1, acc.getOrElse(o._1, 0L) + o._2)