#!/bin/env /bin/scala3

object rational_exercise{
  
  class Rational(x: Int, y: Int): 
    require(y > 0, "denominator must be positive")
    def this(x: Int) = this(x, 1)
    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x.abs,y)
    val numer = x 
    val denom = y 
    def add(r: Rational) =
      Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)
    def mul(r: Rational) = 
      Rational(numer * r.numer, denom * r.denom)
    def neg =
      Rational( -1 * numer, denom)
    def sub(r: Rational) = add(r.neg)
    def less(that: Rational) = 
      numer*that.denom < that.numer * denom
    def max(that: Rational) = 
      if (this.less(that)) that else this
    override def toString = 
      def gcd(a: Int, b: Int): Int =
        if (b == 0) a else gcd(b, a % b)
      s"${numer/gcd(numer.abs,denom)}/${denom/gcd(numer.abs,denom)}"
  end Rational
  
  def main(args: Array[String]) = {
    val x = Rational(1, 3) 
    val y = Rational(-5, 7)
    val z = Rational(3, 2)

    println(x.add(y).mul(z).toString)
    println(x.sub(y).sub(z).toString)
  }
}

