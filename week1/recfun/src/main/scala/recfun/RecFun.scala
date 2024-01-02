package recfun

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
    if c == 0 || r == 0 || c == r then 1
    else if c < 0 || c > r || r < 0 then 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def balanceIter(chars: List[Char], open: Int, closed: Int, last: Char): Boolean =
      if chars.isEmpty then 
        if open == closed && last == ')' then true
        else false
      else if chars.head == ')' && last == ' ' then false
      else if chars.head == '(' 
        then balanceIter(chars.tail, open + 1, closed, '(')
      else if chars.head == ')' 
        then balanceIter(chars.tail, open, closed + 1, ')')
      else if chars.head != '(' || chars.head != ')' 
        then balanceIter(chars.tail, open, closed, last)
      else false
    balanceIter(chars, 0, 0, ' ')

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if money <= 0 then 0
    def countChangeIter(money: Int, coins: List[Int]): Int =
      if money == 0 then 1
      else if coins.isEmpty || money < 0 then 0
      else countChangeIter(money - coins.head, coins) + countChangeIter(money, coins.tail)
    countChangeIter(money, coins)