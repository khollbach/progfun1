object Main {
  def main(args: Array[String]) = {
    val three = new Succ(new Succ(new Succ(Zero)))
    val four = three.successor

    println(three + four + four - three - four)
  }
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Exception
  def +(that: Nat) = that
  def -(that: Nat) = if (that.isZero) this else throw new Exception
  override def toString = "0"
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat) = n + new Succ(that)
  def -(that: Nat) = if (that.isZero) this else n - that.predecessor
  override def toString = s"s($n)"
}
