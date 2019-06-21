object Main {
  def main(args: Array[String]): Unit = {
    println(nested(5))
  }

  def isPrime(n: Int): Boolean =
    n >= 2 && (2 to n - 1 forall (x => n % x != 0))

  def nested(n: Int): List[(Int, Int)] = {
    /** Find all $1 \le j < i < n$ s.t. i + j is prime. */
    val pairs =
      1 until n flatMap (i =>
        1 until i map (j => (i, j)))

    (pairs filter { case (x, y) => isPrime(x + y) }).toList
  }
}
