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
      if (count < 0)  return false 

      if (chars.isEmpty) count == 0
      else {
        if (chars.head == '(') balanceUsingCount(count + 1, chars.tail)
        else if (chars.head == ')') balanceUsingCount(count - 1, chars.tail)
        else balanceUsingCount(count, chars.tail)
      }
    }

    balanceUsingCount(0, chars)
  }

  /**
   * Exercise 3
   * f(n) = sigma f(n - k*c)
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeRec(money:Int, coins: List[Int]): Int = {

      if (money == 0) return 1

      if (coins.isEmpty) return 0

      var noOfWays: Int = 0
      val denomination:Int = coins.head
      var k: Int = 0
      while (k * denomination <= money) {
    	  noOfWays = noOfWays + countChangeRec(money - k * denomination, coins.tail)
    	  k = k+1
      }
      noOfWays
    }

    countChangeRec(money, coins)
  }
}
