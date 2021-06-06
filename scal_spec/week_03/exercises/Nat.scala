#!/bin/env /bin/scala3
/**
 * A few definitions for working with Integer sets
 */

object Nat_exercise{
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  end Nat

  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = ???
    def successor: Nat = Succ(this)
    def + (that: Nat): Nat = that
    def - (that: Nat): Nat = if (that.isZero) this else ???
    override def toString = "Zero"
  end Zero
      
  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    def successor: Nat = Succ(this)
    def + (that: Nat): Nat = Succ(n + that)
    def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
    override def toString = s"Succ($n))"

  end Succ

  }
  def main(args: Array[String]): Unit = {
    val one = Nat_exercise.Succ(Nat_exercise.Zero)
    val two = Nat_exercise.Succ(Nat_exercise.Succ(Nat_exercise.Zero))
    println(two + one)
    }

