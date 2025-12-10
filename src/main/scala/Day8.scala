package day8

case class JB(x: Int, y: Int, z: Int):
    def directLine(other: JB): Double =
        Math.sqrt(Math.pow((other.x - x).toDouble, 2) + Math.pow((other.y - y).toDouble, 2) + Math.pow((other.z - z).toDouble, 2))

def solve1(lines: List[String]): Int =
    val ls = lines
        .map: line =>
            val ps = line
                .split(",")
            JB(ps(0).toInt, ps(1).toInt, ps(2).toInt)

    val a = calCList(ls, 0)
        .map(o => (Set(o._1, o._2) -> o._3))
        .toMap // remove duplicate
        .toList
        .sortBy(_._2)
        .take(10)
        // .take(1000)
        .foldLeft(Nil)((acc, cur) =>
            updateCircit(acc, cur._1)
        )
    println(a)
    println(a.map(_.size).sorted) 
    0

def calCList(jbs: List[JB], idx: Int): List[(JB, JB, Double)] =
    if idx >= jbs.length then
        Nil
    else
        val head = jbs(idx)
        val tail = jbs.take(idx) ++ jbs.drop(idx + 1)
        (head *: calClosest(head, tail)) :: calCList(jbs, idx + 1)

def calClosest(jb: JB, jbs: List[JB]): (JB, Double) =
    jbs
        .filter(_ != jb)
        .map: other =>
            (other, jb.directLine(other))
        .minBy(_._2)

def updateCircit(jbs: List[Set[JB]], jb: Set[JB]): List[Set[JB]] =
    jbs match
        case Nil =>
            List(jb)
        case head :: tail =>
            if head.intersect(jb).size > 0 then
                head.concat(jb) :: tail
            else
                head :: updateCircit(tail, jb)