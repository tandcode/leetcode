package com.leetcode.p1992

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def findFarmland(land: Array[Array[Int]]): Array[Array[Int]] = {
    land
      .map(a => count(a)).zipWithIndex
      .flatMap { case (tuples, i) => tuples.map(_ -> i) }
      .groupBy(t => t._1)
      .map { case (ints, array) =>
        val startRow = array.map(_._2).min
        Array(ints._1, startRow, ints._2 + ints._1 - 1, array.length + startRow - 1)
      }
      .toArray
  }

  def count(ints: Array[Int], i: Int = 0, start: Int = 0, total: Int = 0, prevIs1: Boolean = false,
            res: ArrayBuffer[(Int, Int)] = ArrayBuffer.empty): Array[(Int, Int)] = {
    val endOfArray = i >= ints.length
    val currIs1 = !endOfArray && ints(i) == 1
    if (!currIs1 && prevIs1) res += (start -> total)

    if endOfArray then res.toArray
    else count(ints, i + 1, if currIs1 && !prevIs1 then i else start, if currIs1 then total + 1 else 0, currIs1, res)
  }


  def main(args: Array[String]): Unit = {
    print2dArray(
      findFarmland(
        Array(
          Array(1, 0, 0),
          Array(0, 1, 1),
          Array(0, 1, 1)
        )
      )
    )
    println()
    print2dArray(
      findFarmland(
        Array(
          Array(1, 1),
          Array(1, 1)
        )
      )
    )
    println()
    print2dArray(
      findFarmland(
        Array(
          Array(0)
        )
      )
    )
    println()
    print2dArray(
      findFarmland(
        Array(
          Array(1, 0, 0, 0),
          Array(0, 1, 1, 1),
          Array(0, 1, 1, 1),
          Array(1, 0, 0, 0),
          Array(1, 0, 1, 0),
          Array(1, 0, 1, 0),
        )
      )
    )
    println()
    //    println(count(Array(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1)).mkString("Array(", ", ", ")"))
  }

  private def print2dArray(array: Array[Array[Int]]): Unit = {
    array
      .foreach(l => println(l.mkString(", ")))
  }
}

