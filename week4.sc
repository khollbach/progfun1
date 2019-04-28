object Main {
  def main(args: Array[String]) = {
    println("hello")

    val list = new Cons(10, new Cons(20, new Cons(30, new Nil)))

    println(nth(0, list))
    println(nth(1, list))
    println(nth(2, list))
  }

  def nth[T](n: Int, xs: List[T]): T = {
    if (n < 0) throw new IndexOutOfBoundsException("Negative index: " + n)
    else if (xs.isEmpty)
      throw new IndexOutOfBoundsException("Index too large by: " + (n+1))
    else if (n == 0) xs.head
    else nth(n-1, xs.tail)
  }
}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
