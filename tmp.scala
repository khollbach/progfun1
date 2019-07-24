object Main {
  def main(args: Array[String]): Unit = {
    println()
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }

    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    ???
  }
}
