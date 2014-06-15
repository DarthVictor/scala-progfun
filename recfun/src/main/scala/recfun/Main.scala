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

    println("Counting Change")
    println("asserting for 3")
    println(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
      if (c < 0 || c > r || r < 0)
        0
      else if (c == 0 || c == r || r == 0)
        1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    //var balance = 0;
    def loop (chars: List[Char], balance: Int): Boolean =
      if (balance < 0)
        false
      else if (chars.isEmpty)
        (balance == 0)
      else {
        loop(chars.tail, balance + {
            if (chars.head == '(' ) 1
            else if (chars.head == ')' ) -1
            else 0
          })
      }
    loop(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0)
      0
    else if (coins.isEmpty)
        if (money == 0) 1
        else 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)



}
