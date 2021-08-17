#!/bin/env /bin/scala3

class Polynom(nonZeroTerms: Map[Int, Double]):
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = nonZeroTerms.withDefaultValue(0.0)

  def + (other: Polynom): Polynom = 
    Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

  override def toString =
    val termStrings = 
      for (exp, coeff) <- terms.toList.sorted.reverse
      yield
        val exponent = if exp == 0 then "" else s"x^$exp"
        val sign = if coeff >= 0 then " + " else " - "
        val abs_coeff = coeff.abs
        s"$sign $abs_coeff$exponent"
    if terms.isEmpty then "0"
    else termStrings.mkString(" ")

def main(args: Array[String]): Unit = {
  val f = Polynom( 0 -> 2, 1 -> -3, 2 -> 1)
  println(f.toString)
  }
