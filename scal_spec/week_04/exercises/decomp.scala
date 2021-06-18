#!/bin/env /bin/scala3
/**
 * A few definitions for working with Integer sets
 */

object pme{
  
  trait Expr
  object Expr:
    case class Number(n: Int) extends Expr
    case class Sum(e1: Expr, e2: Expr) extends Expr
    case class Var(x: Int) extends Expr
    case class Prod(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match
     case Expr.Number(n) => n
     case Expr.Sum(e1, e2) => eval(e1) + eval(e2)
     case Expr.Prod(e1, e2) => eval(e1) * eval(e2)

  def show(e: Expr): String = e match
    case Expr.Number(n) => n.toString
    case Expr.Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Expr.Prod(e1, e2) => e1 match
      case Expr.Sum(e2, e3) => s"(${show(e1)}) * ${show(e2)}"
      case _ => s"${show(e1)} * ${show(e2)}"



  def main(args: Array[String]): Unit = {
    val x = pme.Expr.Number(3)
    val y = pme.Expr.Number(7)
    val z = pme.Expr.Sum(x, y)
    println(pme.eval(pme.Expr.Sum(x, y)))
    println(pme.show(pme.Expr.Prod(pme.Expr.Sum(pme.Expr.Number(2),pme.Expr.Number(6)),z)))
    println(pme.show(pme.Expr.Prod(pme.Expr.Prod(pme.Expr.Number(2),pme.Expr.Number(6)),z)))
    }

}

