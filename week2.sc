#!/usr/bin/env scala

object firstHalf extends App {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + a)
    }
    loop(a, 0)
  }

  def sum_(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum_(f)(a + 1, b)

  def sum__(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def fact(n: Int): Int = product(x => x)(1, n)

  def product_(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) * acc)
    loop(a, 0)
  }

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, combine(f(a), acc))
    loop(a, zero)
  }

  def fixedPoint(f: Double => Double)(initialGuess: Double): Double = {
    val eps = 0.0001
    def isClose(x: Double, y: Double) =
      math.abs((x - y) / x) < eps
    def loop(guess: Double): Double = {
      val nextGuess = f(guess)
      if (isClose(guess, nextGuess)) nextGuess
      else loop(nextGuess)
    }
    loop(initialGuess)
  }

  fixedPoint(x => 1 + x/2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1)

  println(sqrt(2))
}
//firstHalf.main(args)

object secondHalf extends App {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  println(x)
  println(x.numer)
  println(x.denom)
  println(x + y)
  println(x - y - z)
  println(y + y)
  println(x < y)
  println(x max y)
  println(new Rational(2))
}
secondHalf.main(args)

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  def < (that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom* that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  override def toString = numer + "/" + denom
}

// (((a + b) ^? (c ?^ d)) less ((a ==> b) | c))
