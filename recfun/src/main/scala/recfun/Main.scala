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

  def pascal(c: Int, r: Int): Int =
    if (r < 0 || c < 0 || c > r) 0
    else if (r == 0 && c == 0) 1
    else pascal(r = r-1, c = c-1) + pascal(r = r-1, c = c)

  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], depth: Int): Boolean =
      if (chars.isEmpty) depth == 0
      else {
        val c = chars.head
        val rest = chars.tail
        if (c == '(') balanceHelper(rest, depth + 1)
        else if (c == ')') depth > 0 && balanceHelper(rest, depth - 1)
        else balanceHelper(rest, depth)
      }

    balanceHelper(chars, depth = 0)
  }

  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else {
      val c = coins.head
      countChange(money, coins.tail) +
        countChange(money - c, coins)
    }
}
