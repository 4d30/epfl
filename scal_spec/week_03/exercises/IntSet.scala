#!/bin/env /bin/scala3
/**
 * A few definitions for working with Integer sets
 */

object IntSet_exercise{
  abstract class IntSet:
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  end IntSet

  object IntSet:
    def apply(): IntSet = Empty
    def apply(x: Int) = Empty.incl(x)
    def apply(x: Int, y: Int) = Empty.incl(x).incl(y)
  end IntSet
  
  object Empty extends IntSet:
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def union(other: IntSet): IntSet = other
  end Empty

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet: 
    def contains(x: Int): Boolean = 
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true
    def incl(x: Int): IntSet = 
      if (x < elem) NonEmpty(elem, left.incl(x), right)
      else if (x > elem) NonEmpty(elem, left, right.incl(x))
      else this
    def union(other: IntSet): IntSet =
      left.union(right).union(other).incl(elem)
  end NonEmpty
  
  def main(args: Array[String]): Unit = {
    println(IntSet(2,3))

    }
}

