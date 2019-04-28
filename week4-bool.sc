//package idealized.scala

object Main {
  def main(args: Array[String]) = {
    val t: Boolean = mytrue
    val f: Boolean = myfalse

    val shouldBeTrue = ((t && f) || t) == f.unary_bang

    println("probably fine")
  }
}

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: => Boolean): Boolean = ifThenElse(x, myfalse)
  def ||(x: => Boolean): Boolean = ifThenElse(mytrue, x)
  def unary_bang: Boolean = ifThenElse(myfalse, mytrue)

  def ==(x: Boolean): Boolean = ifThenElse(x, x.unary_bang)
  def !=(x: Boolean): Boolean = ifThenElse(x.unary_bang, x)

  def <(x: Boolean): Boolean = ifThenElse(myfalse, x)
}

object mytrue extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object myfalse extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = e
}
