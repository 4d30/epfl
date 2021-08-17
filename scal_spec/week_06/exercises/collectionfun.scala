#!/bin/env /bin/scala3
/**
 * A few definitions for working with Integer sets.
 */

object colfun{
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = 
    xs.zip(ys).map(_*_).sum

  def isPrime(n: Int): Boolean =
    (2 until n).forall(n % _ != 0)

  def main(args: Array[String]): Unit = {
    val M = 4
    val N = 6
    val xs = Vector[Double](1, 2, 3)
    val ys = Vector[Double](-4, 8, -32)
    val out = (1 to M).flatMap( x => (1 to N).map(y => (x,y)))
    val out2 = colfun.scalarProduct(xs, ys)
    val out3 = isPrime(173)
    println(out3)
 
    }

}
