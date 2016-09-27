package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def comp(x: Int, y:Int):Boolean = { return (x==y) }
  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(comp(c,0)) return 1
      if(comp(c,r)) return 1
      return pascal(c,r-1) + pascal(c-1,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balanceHelper(x: Int, chars: List[Char]): Boolean = {
      if(x<0) return false;
      if(chars.isEmpty) return comp(x,0);
      if(chars.head == '(') return balanceHelper(x+1,chars.tail);
      if(chars.head == ')') return balanceHelper(x-1,chars.tail);
      return balanceHelper(x,chars.tail);
    }
    def balance(chars: List[Char]): Boolean = {
      return balanceHelper(0,chars);
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(comp(money,0)) return 1
      if(money<0) return 0
      if(coins.isEmpty && money>0) return 0
      return countChange(money,coins.tail) + countChange(money - coins.head,coins);
    }
  }
