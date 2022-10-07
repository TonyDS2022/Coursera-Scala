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
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def balance_tailrec(chars: List[Char], open: Int): Boolean =
      if chars.isEmpty then
        open == 0
      else
        val new_open: Int = chars.head match {
          case '(' => open + 1
          case ')' => open - 1
          case  _  => open
        }
        if new_open < 0 then false
        else balance_tailrec(chars.tail, new_open)
    balance_tailrec(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def count_rec(coins: List[Int], sum: Int, cnt: Int): Int =
      if coins.isEmpty then
        cnt
      else
        if sum > 0 then
          count_rec(coins, sum - coins.head, cnt) + count_rec(coins.tail, sum, cnt)
        else
          if sum == 0 then
            cnt + 1
          else cnt
    count_rec(coins.sorted, money, 0)