package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }

    val coins = List(5,10,20,50,100,200,500)
    Console println countChange(301, coins)
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
      @tailrec
      def rec(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) count == 0
        else {
          val char = chars.head
          val newCount = 
            if (char == '(') count + 1
            else if (char == ')') count - 1
            else count

          if (count < 0) false
          else rec(chars.tail, newCount)
        }
      }

      rec(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
      def sum(amount: Int, f: (Int, List[Int], List[Int]) => Int, coins: List[Int], comb: List[Int]): Int = {
        if (coins.isEmpty) 0
        else
          f(amount, coins, comb) + 
          sum(amount, f, coins.tail, comb)
      }

      def count(amount: Int, coins: List[Int], comb: List[Int]): Int = {
        if (coins.isEmpty) 0
        else {
          val biggest = coins.head
          val remainder = amount - biggest
          if (remainder < 0) 0
          else if (remainder == 0) 1
          else sum(remainder, count, coins, biggest :: comb)
        }
      }

      val sortedCoins = sort(coins) 
      sum(money, count, sortedCoins, Nil)
    }

    def insertEl(elem: Int, list: List[Int]): List[Int] = {
      if (list.isEmpty) list ::: elem :: Nil
      else {
        val head = list.head
        if (elem > head) elem :: list
        else {
          head :: insertEl(elem, list.tail)
        }
      }
    }

    def sort(list: List[Int]) = {
      insertEl(list.head, list.tail)
      
      def buildList(from: List[Int], to: List[Int]): List[Int] = {
        if (from.isEmpty) to
        else buildList(from.tail, insertEl(from.head, to))
      }

      buildList(list, Nil)
    }
  }
