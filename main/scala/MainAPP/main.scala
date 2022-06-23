package MainAPP

import Expression._
import Expression.calculatorTools._

object main {
  def main(args: Array[String]): Unit = {
    val num1 = Number(3)
    val num2 = Number(7)
    val num3 = Number(1)
    val num4 = Number(17.3)
    val num5 = Number(5.7)
    val num6 = Number(3.2)
    val x = Variable

    println("Test de la fonction showExpr")
    val expr1 = Mul(num1, Add(num2, num3))
    println(showExpr(expr1))
    val expr2 = Add(Mul(num1, x), num4)
    println(showExpr(expr2))
    val expr3 = Add(Sin(x), Cos(x))
    println(showExpr(expr3))
    val expr4 = Sin(Add(Mul(Number(2),x),num6))
    val expr5 = Add(Mul(num1, x),num5)
    val exprFinal = Add(expr4, expr5)
    println(showExpr(exprFinal))

    println()
    println("Test de la fonction eval avec x = 2")
    println(showExpr(expr1))
    println(eval(expr1)(2))
    println(showExpr(expr2))
    println(eval(expr2)(2))
    println(showExpr(expr3))
    println(eval(expr3)(2))
    println(showExpr(expr4))
    println(eval(expr4)(2))
    println(showExpr(exprFinal))
    println(eval(exprFinal)(2))

    println()
    val expr7 = Add(num1, num2) // 3 + 7
    val expr8 = Add(num1, Number(0)) // 3 + 0
    val expr9 = Add(Variable, Variable) // x + x
    val expr10 = Mul(num1, num2) // 3 * 7
    val expr11 = Mul(num1, Number(0)) // 3 * 0
    val expr12 = Mul(num1, Number(1)) // 3 * 1

    println("Test de la fonction simplify")
    println(showExpr(expr7))
    println(showExpr(simplify(expr7)))
    println(showExpr(expr8))
    println(showExpr(simplify(expr8)))
    println(showExpr(expr9))
    println(showExpr(simplify(expr9)))
    println(showExpr(expr10))
    println(showExpr(simplify(expr10)))
    println(showExpr(expr11))
    println(showExpr(simplify(expr11)))
    println(showExpr(expr12))
    println(showExpr(simplify(expr12)))

    println()
    println("Test du parser")
    val stringExpr = "2+5*9*x+1"
    println(s"expression a parser : $stringExpr")
    println(showExpr(ExpressionParser.parse(stringExpr)))
//    val expr1 = Add(num1, num2)
//    val expr2 = Add(expr1, num1)
//    val expr3 = Mul(expr2,num3)
//    val expr4 = Add(expr3, num2)
//    val expr5 = sin(expr4)
//    val expr6 = cos(expr4)
//    val expr7 = Mul(expr5, expr6)
//    println(showExpr(expr5))
//    println(showExpr(expr6))

//    val expr2 = Mul(expr1, num1)
//    val expr3 = sin(expr2)
//    println(showExpr(expr2))
//    println(showExpr(expr3))
  }
}
