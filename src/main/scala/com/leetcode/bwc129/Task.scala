package com.leetcode.bwc129

object Task {
  def canMakeSquare(grid: Array[Array[Char]]): Boolean = {
    var res = false

    for (row <- 0 to 1) {
      for (col <- 0 to 1) {
        if (count2x2(grid, row, col, 'W') != 2) {
          res = true
        }
      }
    }

    res
  }

  private def count2x2[T](grid: Array[Array[T]], row: Int, col: Int, compare: T) = {
    List(
      grid(row)(col),
      grid(row)(col + 1),
      grid(row + 1)(col),
      grid(row + 1)(col + 1)
    ).count(_ == compare)
  }

  def numberOfRightTriangles(grid: Array[Array[Int]]): Long = {
    val rows = grid.map(a => a.count(_ == 1) - 1)
    val cols = grid(0).indices.map(r => grid.map(_(r)).sum - 1)

    val columns = grid(0).indices
    
    grid.indices
      .flatMap(ri => columns.map(ci => if grid(ri)(ci) == 1 then rows(ri).toLong * cols(ci) else 0L))
      .sum
  }

  def numberOfStableArrays(zero: Int, one: Int, limit: Int): Int = {
    val total = zero + one
    val combos = factorial(total) / (factorial(zero) * factorial(one))

    combos *
    0
  }

  def factorial(x: Int): Int =
    def fact(x: Int, accumulator: Int): Int =
      if x <= 1 then accumulator
      else fact(x - 1, x * accumulator)

    fact(x, 1)

  def main(args: Array[String]): Unit = {
    println(canMakeSquare(
      Array(
        Array('B', 'W', 'B'),
        Array('B', 'W', 'W'),
        Array('B', 'W', 'B')
      )
    ))

    println(canMakeSquare(
      Array(
        Array('B', 'W', 'B'),
        Array('B', 'W', 'W'),
        Array('B', 'W', 'W')
      )
      ))

    println(canMakeSquare(
      Array(
        Array('B', 'W', 'B'),
        Array('W', 'B', 'W'),
        Array('B', 'W', 'B')
      )
    ))

    println(canMakeSquare(
      Array(
        Array('B', 'B', 'B'),
        Array('B', 'B', 'B'),
        Array('B', 'B', 'B')
      )
    ))

    println(canMakeSquare(
      Array(
        Array('B', 'B', 'B'),
        Array('W', 'W', 'W'),
        Array('B', 'B', 'B')
      )
    ))

    println(numberOfRightTriangles(
      Array(
        Array(0, 0, 0),
        Array(0, 0, 0),
        Array(0, 0, 0)
      )
    ))

    println(numberOfRightTriangles(
      Array(
        Array(0, 1, 0),
        Array(0, 1, 1),
        Array(0, 1, 0)
      )
    ))

    println(numberOfRightTriangles(
      Array(
        Array(1, 0, 1),
        Array(1, 0, 0),
        Array(1, 0, 0)
      )
    ))

    println(numberOfRightTriangles(
      Array(
        Array(1, 0, 1),
        Array(1, 1, 1),
        Array(1, 0, 0)
      )
    ))
  }
}
