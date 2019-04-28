object Main {
  def main(args: Array[String]) = {
    val t1 = new NonEmpty(3, new Empty, new Empty)
    val t2 = t1 incl 4
    println(t1)
    println(t2)

    val t3 = new Empty incl 5 incl 3 incl 7
    val t4 = new Empty incl 9 incl 10 incl 6
    val t5 = t3 union t4
    println(t3)
    println(t4)
    println(t5)
  }
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

  def union(other: IntSet): IntSet = {
    left union (right union (other incl elem))
  }
}
