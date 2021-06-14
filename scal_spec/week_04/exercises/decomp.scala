#!/bin/env /bin/scala3
/**
 * A few definitions for working with Integer sets
 */

object pme{
  
  trait Expr
  case class Numberr(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Var(x: Int) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match
     case Numberr(n) => n
     case Sum(e1, e2) => eval(e1) + eval(e2)
     case Prod(e1, e2) => eval(e1) * eval(e2)

  def show(e: Expr): String = e match
    case Numberr(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Prod(e1, e2) => e1 match
      case Sum(e2, e3) => s"(${show(e1)}) * ${show(e2)}"
      case _ => s"${show(e1)} * ${show(e2)}"



  def main(args: Array[String]): Unit = {
    val x = pme.Numberr(3)
    val y = pme.Numberr(7)
    val z = pme.Sum(x, y)
    println(pme.eval(pme.Sum(x, y)))
    println(show(pme.Prod(pme.Sum(pme.Numberr(2),pme.Numberr(6)),z)))
    println(show(pme.Prod(pme.Prod(pme.Numberr(2),pme.Numberr(6)),z)))
    }

}

