#!/bin/env -S ./Nat.scala /bin/scala3 
/**
 * A few definitions for working with Integer sets
 */

import Nat._

object polymorph_exercise{
  def nth[T](xs: List[T], n: Int): T = {
    if (xs.isEmpty) throw IndexOutOfBoundsException()
    def iter[T](xs: List[T], a: Int): T = {
      if (a == n) xs.head
      else if ( a < 0 || a > xs.length - 1) throw IndexOutOfBoundsException()
      else iter(xs.tail, a + 1)}
    iter(xs, 0)
  }
  def main(args: Array[String]): Unit = {
    println("hello world!")
    val l = List(0,1,2,3,4)
    println(nth(l,2))
    }
}

