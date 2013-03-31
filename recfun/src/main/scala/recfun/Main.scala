package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c==0 || c==r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceUsingCount(count : Int, chars: List[Char]) : Boolean = {
      if (count < 0) false 
	  else {
	      if (chars.isEmpty) count == 0
	      else {
	        if (chars.head == '(') balanceUsingCount(count + 1, chars.tail)
	        else if (chars.head == ')') balanceUsingCount(count - 1, chars.tail)
	        else balanceUsingCount(count, chars.tail)
	      }
      }
    }

    balanceUsingCount(0, chars)
  }

  /**
   * Exercise 3
   * f(n) = sigma f(n - k*c)
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeWithKCoins(money:Int, denomination:Int, remainingCoins:List[Int], k:Int):Int = {
      if (k * denomination <= money) 
        countChangeRec(money - k * denomination, remainingCoins) + 
        countChangeWithKCoins(money, denomination, remainingCoins, k+1)
      else
        0
    }

    def countChangeRec(money:Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else countChangeWithKCoins(money, coins.head, coins.tail, 0)
    }

    countChangeRec(money, coins)
  }
}
