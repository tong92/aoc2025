package day5

case class RangePair(first: Long, second: Long):
    def isFresh(id: Long): Boolean =
        id >= first && id <= second
    def isIntersecting(other: RangePair): Boolean =
        !(other.second < first || other.first > second)
    def count =
        second - first + 1

object RangePair:
    def setRange(v: String): RangePair =
        val parts = v.split("-")
        RangePair(parts(0).toLong, parts(1).toLong)


def solve1(lines: List[String]): Long =
    val rangePairs = lines
        .filter(_.contains("-"))
        .map(RangePair.setRange)
    lines.filterNot(_.contains("-")).filterNot(_.isEmpty()).map(
        id =>
            rangePairs
            .find(_.isFresh(id.toLong))
    ).count(_.isDefined)

def combine(acc: List[RangePair], rp: RangePair): List[RangePair] =
    acc match
        case head :: tail =>
            if head.isIntersecting(rp) then
                combine(tail, RangePair(
                    Math.min(head.first, rp.first),
                    Math.max(head.second, rp.second)
                ))
            else
                head :: combine(tail, rp)
        case _ => rp :: acc

def solve2(lines: List[String]): Long =
    lines
        .filter(_.contains("-"))
        .map(RangePair.setRange)
        .foldLeft(Nil)(combine)
        .map(_.count)
        .sum