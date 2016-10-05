package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    Console println countChange(100, Nil)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
 
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def updateCount(char: Char, count: Int) = 
        if (char == '(') count + 1
        else if (char == ')') count - 1
        else count

      @tailrec
      def rec(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) count == 0
        else {
          if (count < 0) false
          else rec(chars.tail, updateCount(chars.head, count))
        }
      }

      rec(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
      def sum(amount: Int, count: (Int, List[Int]) => Int, coins: List[Int]): Int = 
        if (coins.isEmpty) 0
        else count(amount, coins) + sum(amount, count, coins.tail)

      def count(amount: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0
        else {
          val biggest = coins.head
          val remainder = amount - biggest
          if (remainder < 0) 0
          else if (remainder == 0) 1
          else sum(remainder, count, coins)
        }
      }

      val sortedCoins = if(!coins.isEmpty) sort(coins)
                        else Nil
      sum(money, count, sortedCoins)
    }

    private def insertEl(elem: Int, list: List[Int]): List[Int] = {
      if (list.isEmpty) elem :: Nil
      else {
        val head = list.head
        if (elem > head) elem :: list
        else head :: insertEl(elem, list.tail)
      }
    }

    private def sort(list: List[Int]) = {
      @tailrec
      def buildList(from: List[Int], to: List[Int]): List[Int] = {
        if (from.isEmpty) to
        else buildList(from.tail, insertEl(from.head, to))
      }

      buildList(list, Nil)
    }
  }
