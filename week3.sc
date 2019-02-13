object Main {
  def main(args: Array[String]) = {
    val t1 = new NonEmpty(3, new Empty, new Empty)
    val t2 = t1 incl 4
    println(t1, t2)
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

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  override def toString = "{" + left + elem + right + "}"

  def union(other: IntSet): IntSet = {
    this
  }

  //def union(other: IntSet) = other match {
    //case Empty => this
    //case o: NonEmpty =>
      //val left =
        //if (o.elem < this.elem) this.left union o.elem
        //else this.left
      //val right =
        //if (o.elem > this.elem) this.right union o.elem
        //else this.right
      //new NonEmpty(this.elem, left, right) union o.left union o.right
  //}
}
