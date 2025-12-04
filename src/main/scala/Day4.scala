package day4

def countRoll(s: String, at: Int): Int =
    (
        if at == 0 then
            s.substring(at, at + 2)
        else if at == s.length - 1 then
            s.substring(at - 1, at + 1)
        else
            s.substring(at - 1, at + 2)
    ).count(_ == '@')

def solve1(input: List[String]): Int =
    val countAdjList = input.map: str =>
        (0 to str.length - 1).map(countRoll(str, _)).toList
    val dummy = countAdjList(0).map(_ => 0)
    val resList = (dummy :: countAdjList ++ List(dummy))
    input.zipWithIndex.map: (str, idx) =>
        str.zipWithIndex.map: (v, jdx) =>
            if v == '@' then 
                resList(idx)(jdx) + resList(idx + 1)(jdx) + resList(idx + 2)(jdx)
            else 9
    .flatten
    .count(_ < 5)

def solve2(ips: List[String]): Int =
    def inner(input: List[String], cnt: Int): Int =
        val countAdjList = input.map: str =>
            (0 to str.length - 1).map(countRoll(str, _)).toList
        val dummy = countAdjList(0).map(_ => 0)
        val resList = (dummy :: countAdjList ++ List(dummy))
        var x = 0
        var nextInput = input.zipWithIndex.map: (str, idx) =>
            str.zipWithIndex.map: (v, jdx) =>
                if v == '@' then 
                    if resList(idx)(jdx) + resList(idx + 1)(jdx) + resList(idx + 2)(jdx) < 5 then
                        x += 1
                        '.'
                    else v
                else v
        .map(_.mkString)
        if x == 0 then cnt
        else inner(nextInput, cnt + x)

    inner(ips, 0)