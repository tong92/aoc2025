package day1

trait DN
case class DnLeft(amount: Int) extends DN
case class DnRight(amount: Int) extends DN

def line2DN(line: String): DN =
  val number = line.tail.toInt
  line.head match
    case 'L' => 
      DnLeft(number)
    case _ => 
      DnRight(number)

def solve1(lines: List[String], cal: Int, cnt: Int): Int =
  lines match
    case Nil => cnt
    case line :: rest =>
      val newCal = line2DN(line) match
        case DnLeft(number)  => 
          val n = cal - number % 100
          if n < 0 then 100 + n else n
        case DnRight(number) => 
          val n = cal + number
          if n > 99 then n % 100 else n

      if newCal == 0 then
        solve1(rest, newCal, cnt + 1)
      else
        solve1(rest, newCal, cnt)

def solve2(lines: List[String], cal: Int, cnt: Int): Int =
  lines match
    case Nil => cnt
    case line :: rest =>
      val (n, c) = line2DN(line) match
        case DnLeft(number)  => rotateLeft(cal, number)
        case DnRight(number) => rotateRight(cal, number)
      solve2(rest, n, cnt + c)

def next(cur: Int, cnt: Int): (Int, Int) =
  if cur == 0 then (0, cnt + 1)
  else if cur < 0 then next(cur + 100, cnt + 1)
  else (cur % 100, cnt + cur / 100)
  
def rotateLeft(cal: Int, number: Int): (Int, Int) =
  val f = if cal == 0 then -1 else 0
  next(cal - number, f)

def rotateRight(cal: Int, number: Int): (Int, Int) =
  next(cal + number, 0)
  
