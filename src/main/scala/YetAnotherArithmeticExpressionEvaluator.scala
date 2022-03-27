
import scala.util.parsing.combinator._

class YetAnotherArithmeticExpressionEvaluator extends JavaTokenParsers
{
  def apply(expression: String): Int = parseAll(expr, expression) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new IllegalArgumentException(s"could not parse '$expression' with error $msg")
  }

  def expr: Parser[Int] = term~rep("+"~term | "-"~term) ^^ sum

  def term: Parser[Int] = number~rep("*"~number | "/"~number) ^^ product

  def number: Parser[Int] = wholeNumber ^^ (_.toInt)

  def sum( in: ~[Int, List[~[String, Int]]])= in match {
    case n0~sgnNumberList => sgnNumberList.foldLeft(n0) {
      case (z, "+"~n) => z+n
      case (z, "-"~n) => z-n
    }
  }

  def product( in: ~[Int, List[~[String, Int]]]) = in match {
    case n0~opNumberList => opNumberList.foldLeft(n0) {
      case (z, "*"~n) => z*n
      case(z, "/"~n) => z/n
    }
  }
}



