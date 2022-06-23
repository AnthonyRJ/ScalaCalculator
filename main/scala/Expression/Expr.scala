package Expression
import scala.annotation.tailrec
import scala.util.parsing.combinator._

sealed trait Expr {}
case class Number(value : Double) extends Expr
//case class Variable(val name : String = "x") extends Expr
case object Variable extends Expr { val x = "x"}
case class Add(left : Expr, right : Expr) extends Expr
case class Mul(left : Expr, right : Expr) extends Expr
case class Sin(value : Expr) extends Expr
case class Cos(value : Expr) extends Expr

object calculatorTools {

  def isNumber(e : Expr) : Boolean = e match{
    case Number(value) => true
    case _ => false
  }

  def isAdd(e : Expr): Boolean = e match{
    case Add(left, right) => true
    case _ => false
  }

  def isMul(e : Expr) : Boolean = e match {
    case Mul(left, right) => true
    case _ => false
  }

  def isVariable(e : Expr) : Boolean = e match {
    case Variable => true
    case _ => false
  }

  // function that show a mathematical expression as a String
  def showExpr(e : Expr): String = e match{
    case Number(value) => value.toString
    case Variable => Variable.x
    case Sin(value) => "sin(" + showExpr(value) + ")"
    case Cos(value) => "cos(" + showExpr(value) + ")"
    case Add(left, right) => "(" + showExpr(left) + "+" + showExpr(right) + ")"
    case Mul(left, right) => "(" + showExpr(left) + "*" + showExpr(right) + ")"
//    case Add(left, right) => if((isAdd(left) || isNumber(left) || isVariable(left)) & (isAdd(right) || isNumber(right) || isVariable(right))){
//      showExpr(left) + "+" + showExpr(right)
//    } else if (isAdd(left) || isNumber(left)){
//      showExpr(left) + "+" + "(" +showExpr(right) + ")"
//    } else {
//      "(" + showExpr(left) + ")" + "+" +showExpr(right)
//    }
//
//    case Mul(left, right) => if((isMul(left) || isNumber(left) || isVariable(left)) & (isMul(right) || isNumber(right) || isVariable(right))){
//      showExpr(left) + "*" + showExpr(right)
//    } else if(isMul(left) || isNumber(left)){
//      showExpr(left) + "*" + "(" +showExpr(right) + ")"
//    }else{
//      "(" + showExpr(left) + ")" + "*" +showExpr(right)
//    }
  }

  // function that compute the result of a mathematical expression e with the given value x for the variable
  def eval(e : Expr)(x : Double) : Double = e match {
    case Number(value) => value
    case Variable => x
    case Mul(left, right) => eval(left)(x) * eval(right)(x)
    case Add(left, right) => eval(left)(x) + eval(right)(x)
    case Sin(value) => Math.sin(eval(value)(x))
    case Cos(value) => Math.cos(eval(value)(x))
  }

  // function that simplify an expression e to its simplest form
  def simplify(e : Expr): Expr = e match{
    case Mul(_, Number(0)) => Number(0)
    case Mul(Number(0), _) => Number(0)
    case Mul(left, Number(1)) => simplify(left)
    case Mul(Number(1), right) => simplify(right)
    case Mul(Number(left), Number(right)) => Number(left * right)

    case Add(left, Number(0)) => simplify(left)
    case Add(Number(0), right) => simplify(right)
    case Add(Number(left), Number(right)) => Number(left + right)
    case Add(Variable, Variable) => Mul(Number(2), Variable)

    case _ => e
  }
}

//object ExpressionParser extends JavaTokenParsers with PackratParsers{
//  lazy val expr: PackratParser[Expr] = (expr <~ "+") ~ term ^^ { case left~right => Add(left,right) } | term
//  lazy val term: PackratParser[Expr] = (term <~ "*") ~ factor ^^ { case left~right => Mul(left, right) } | factor
//  lazy val factor : PackratParser[Expr] = "(" ~> expr <~ ")" | floatingPointNumber ^^ {x => Number(x.toDouble) }
//
//  // Parse the string to a data structure type Expr
//  def parse(text : String): Expr = parseAll(expr, text).get
//
//}

object ExpressionParser extends RegexParsers {
  def expr  : Parser[Expr] = term ~ rep("+" ~ term) ^^ { case number ~ list => list.foldLeft(number) { case (left, "+" ~ right) => Add(left,right)}}

  def term  : Parser[Expr] = factor ~ rep( "*" ~ factor) ^^ { case number ~ list => list.foldLeft(number) { case (left, "*" ~ right) => Mul(left,right)}}

  def factor : Parser[Expr] = number | variable | "(" ~> expr <~ ")"

  def number : Parser[Expr] = """\d+(\.\d*)?""".r ^^ { x => Number(x.toDouble) }

  def variable : Parser[Expr] = "x".r ^^ { _ => Variable}

  def parse(text : String): Expr = parseAll(expr, text).get
}



