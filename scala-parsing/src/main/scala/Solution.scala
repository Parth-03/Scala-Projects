import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
  def eval(e: Expr): Double = e match
  {
    case Num(e: Double)=> e
    case Add(e1: Expr,e2: Expr)=> eval(e1) + eval(e2)
    case Sub(e1: Expr,e2: Expr)=> eval(e1) - eval(e2)
    case Mul(e1: Expr,e2: Expr)=> eval(e1) * eval(e2)
    case Div(e1: Expr,e2: Expr)=> eval(e1) / eval(e2)
    case Exponent(e1: Expr,e2: Expr)=> Math.pow(eval(e1),eval(e2))
  }
}

object ArithParser extends ArithParserLike {

  // number: PackratParser[Double] is defined in ArithParserLike

  lazy val atom: PackratParser[Expr] = 
  "(" ~ expr ~ ")" ^^ 
  {
    case _ ~ e ~ _=> e
  }| number ^^ 
  {
    case x=> Num(x)
  } 

  lazy val exponent: PackratParser[Expr] = 
  exponent ~ "^" ~ atom ^^ 
  {
    case e1 ~ "^" ~ e2=> Exponent(e1,e2)
  } | atom

  lazy val add: PackratParser[Expr] = 
  add ~ "+" ~ mul ^^ 
  {
    case e1 ~ "+" ~ e2=> Add(e1,e2)
  } | add ~ "-" ~ mul ^^
  {
    case e1 ~ "-" ~ e2=> Sub(e1,e2) 
  } | mul

  lazy val mul: PackratParser[Expr] = 
  mul ~ "*" ~ exponent ^^
  {
    case e1 ~ "*" ~ e2=> Mul(e1,e2)
  } | mul ~ "/" ~ exponent ^^
  {
    case e1 ~ "/" ~ e2=> Div(e1,e2)
  } | exponent

  lazy val expr: PackratParser[Expr] = add
}

object ArithPrinter extends ArithPrinterLike {
  def print(e: Expr): String = e match
  {
    case Num(e: Double)=> e.toString
    case Add(e1: Expr,e2: Expr)=> "(" + print(e1) + "+" + print(e2) + ")"
    case Sub(e1: Expr,e2: Expr)=> "(" + print(e1) + "-" + print(e2) + ")"
    case Mul(e1: Expr,e2: Expr)=> "(" + print(e1) + "*" + print(e2) + ")"
    case Div(e1: Expr,e2: Expr)=> "(" + print(e1) + "/" + print(e2) + ")"
    case Exponent(e1: Expr,e2: Expr)=> "(" + print(e1) + "^" + print(e2) + ")"
  }
}