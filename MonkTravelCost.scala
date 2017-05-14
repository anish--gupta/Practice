import scala.io.StdIn
import scala.annotation.tailrec

object HelloWorld {
    val minVal = 0.toLong
    def main(args: Array[String]) {
        val num = StdIn.readInt
        (1 to num) foreach(_ => compute)
    }
    
    def compute: Unit = {
        val destCount = StdIn.readInt
        val costList: List[Long] = StdIn.readLine.split(" ").map(_.toLong) toList
        val reqList: List[Long] = StdIn.readLine.split(" ").map(_.toLong) toList
        
        println(computeCost(costList, reqList))
    }
    
    @tailrec
    def computeCost(costList: List[Long], reqList: List[Long], acc: Long = 0): Long = {
        def traverseAsc(cost: Long, reqList: List[Long]): Long = {
            val distance = reqList.foldLeft(minVal)(_ + _)
            cost * distance
        }
        
        def traverseDesc(costList: List[Long], reqList: List[Long]): Long = {
            val costs = costList.zip(reqList) map { x =>
                x._1 * x._2
            } 

            costs.foldLeft(minVal)(_ + _)
        }
        
        @tailrec
        def findMin(costList: List[Long], min: Long = -1, currIdx:Int = 0, minIdx: Int = 0, asc: Boolean = true, desc: Boolean = true): (Int, Boolean, Boolean) = {
            val minimum = if (min == -1) costList.head else min

            costList match {
                case head::tail =>
                    val (ascd, descd) = if (currIdx > 0) {
                        val currStat = head < minimum
                        (asc && !currStat, desc && currStat)
                    } else (asc, desc)
                    if (head < minimum) findMin(tail, head, currIdx + 1, currIdx, ascd, descd)
                    else findMin(tail, minimum, currIdx + 1, minIdx, ascd, descd)
                    
                case Nil =>
                    (minIdx, asc, desc)
            }
        }
        
        val (minIdx, asc, desc) = findMin(costList)
        if (desc) traverseDesc (costList, reqList) + acc
        else if (asc) traverseAsc (costList.head, reqList) + acc
        else {
            val minCost = costList.drop(minIdx).head
            val minReqs = reqList.drop(minIdx)
            val trailingCost = traverseAsc(minCost, minReqs)
            computeCost(costList.dropRight(costList.length - minIdx),
                        reqList.dropRight(costList.length - minIdx),
                        acc + trailingCost)
        }
    }   
}