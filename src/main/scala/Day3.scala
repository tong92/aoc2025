package day3

def findLargest(str: List[Char], a: Char, b: Char): Int =
    str match
        case head :: tail :: res =>
            if (head > a) then 
                findLargest(tail :: res, head, tail)
            else if (tail > b) then 
                findLargest(tail :: res, a, tail)
            else findLargest(tail :: res, a, b)
        case _ => (a - 48) * 10 + (b - 48)

def solve1(input: List[String]): Int =
    input.map:
        v => findLargest(v.toList, '0', '0')
    .sum

def findBest(str: List[Char], cur: List[Char]): Long =
    str match
        case head :: res =>
            findBest(res, makeBest(cur :+ head, Nil))
        case _ => cur.mkString.toLong

def makeBest(cur: List[Char], hs: List[Char]): List[Char] =
    cur match
        case head :: tail :: res =>
            if head < tail then
                hs ::: tail :: res
            else
                makeBest(tail :: res, hs :+ head)
        case _ => (hs ::: cur).take(12)

def solve2(input: List[String]): Long =
    input.map:
        v => 
            findBest(v.toList, ("0" * 12).toList)
    .sum