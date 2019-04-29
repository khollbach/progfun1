object Main {
  def main(args: Array[String]) = {
    val list = List(9, 3, 7, 4, 5)
    println(isort(list))
  }

  def isort(list: List[Int]): List[Int] = list match {
    case List() => List()
    case x :: xs => insert(x, isort(xs))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys =>
      if (x <= y) x :: xs
      else y :: insert(x, ys)
  }
}
