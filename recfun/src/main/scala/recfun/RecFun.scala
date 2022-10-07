package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    @tailrec
    def pascal_tailrec(c: Int, prod: Double): Int =
      if c == 0 then
        prod.toInt
      else
        pascal_tailrec(c - 1, prod * (r - c + 1) / c)
    if c == 0 || c == r then
      1
    else
      pascal_tailrec(c,1.0)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
