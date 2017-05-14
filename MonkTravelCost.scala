/****
  * Compute the travel cost of the monk
  * 1
  * 5
  * 10 11 12 13 9 // Cost array
  * 1 2 3 4 5 // Requirement to reach the next station
  *
  * Find local minima - 9 (9*5)
  * Rest of the left array is in ascending order
  * Find Minima 10
  * Find distance 10 (1 + 2 + 3 + 4)
  * Total = 10 * 10 + 45 = 145
  * In order to compute the minimum cost, we find the minima of the array. All the right elements distance,
  * fuel can be refilled from this pump itself. For all the left ones, we again have to compute the minima.
  * Continue till either the residual cost array is in ascending order or descending order.
  *
  * Question
  *
  */

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
  def computeCost(costList: List[Long], reqList: List[Long], acc: Long = 0, min: Long = -1): Long = {
    val minimum = if (min == -1) costList.head else min
    costList match {
      case head::tail =>
        if (head < minimum) {
          val accumulated = acc + reqList.head * head
          computeCost(tail, reqList.tail, accumulated, head)
        } else {
          val accumulated = acc + reqList.head * minimum
          computeCost(tail, reqList.tail, accumulated, minimum)
        }

      case Nil => acc
    }
  }
}
