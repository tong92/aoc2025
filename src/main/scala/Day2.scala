package day2

case class IDRange(first: Long, last: Long):
    def invalidIDs: List[Long] =
        for 
            id <- (first to last).toList
            valid = ID(id).isValid
            res = valid match
                case Left(v)  => v
                case Right(_) => 0
        yield res
    def invalidIDsUpgrade: List[Long] =
        for 
            id <- (first to last).toList
            valid = ID(id).isValidUpgrade
            res = valid match
                case Left(v)  => v
                case Right(_) => 0
        yield res

case class ID(value: Long):
    def isValid: Either[Long, Long] =
        this.value.toString.splitAt(this.value.toString.length / 2) match
            case (left, right) =>
                if left == right then Left(this.value)
                else Right(this.value)
    def isValidUpgrade: Either[Long, Long] =
        val half = this.value.toString.length / 2
        dupCheck(this.value.toString, half, 1)
        
def dupCheck(str: String, half: Int, idx: Int): Either[Long, Long] =
    if idx > half then return Right(str.toLong)
    
    val (left, right) = str.splitAt(idx)
    right.replaceAll(left, "") match
        case "" =>
            Left(str.toLong)
        case _ =>
            dupCheck(str, half, idx + 1)
    

def solve1(input: String): Long =
    input.split(",").map:
        str =>
            val bounds = str.split("-").map(_.toLong)
            IDRange(bounds(0), bounds(1)).invalidIDs
    .flatten
    .sum
def solve2(input: String): Long =
    input.split(",").map:
        str =>
            val bounds = str.split("-").map(_.toLong)
            IDRange(bounds(0), bounds(1)).invalidIDsUpgrade
    .flatten
    .sum