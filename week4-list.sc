object Main {
  def main(args: Array[String]) = {
    println("hello")

    val list = new Cons(10, new Cons(20, new Cons(30, new Nil)))

    println(nth(0, list))
    println(nth(1, list))
    println(nth(2, list))

    println(List())
    println(List(10))
    println(List(10, 20))
    println(List(10, 20, 30))
  }

  def nth[T](n: Int, xs: List[T]): T = {
    if (n < 0) throw new IndexOutOfBoundsException("Negative index: " + n)
    else if (xs.isEmpty)
      throw new IndexOutOfBoundsException("Index too large by: " + (n+1))
    else if (n == 0) xs.head
    else nth(n-1, xs.tail)
  }
}

object List {
  def apply[T](): List[T] = new Nil
  def apply[T](x: T): List[T] = new Cons(x, new Nil)
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))
  def apply[T](x: T, y: T, z: T): List[T] =
    new Cons(x, new Cons(y, new Cons(z, new Nil)))
}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString = s"($head,$tail)"
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString = "."
}
