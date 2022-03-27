
import java.nio.file.{Files, Paths}
import scala.util.parsing.combinator._

class YetAnotherArithmeticExpressionEvaluator extends JavaTokenParsers
{
  def apply(expression: String): Int = parseAll(expr, expression) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new IllegalArgumentException(s"could not parse '$expression' with error $msg")
  }

  def expr: Parser[Int] = term~rep("+"~term | "-"~term) ^^ sum

  def term: Parser[Int] = number~rep("*"~number | "/"~number) ^^ product

  def number: Parser[Int] = wholeNumber ^^ (_.toInt) | "("~>expr<~")"

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

import scala.io.Source
import scala.util.Using
object YetAnotherArithmeticExpressionEvaluator extends App {
  if (args.length == 0) {
    println("Please provide the name of a file with one expression per line.")
    System.exit(1)
  } else {
    val filename = args(0)
    if( Files.exists(Paths.get(filename)) && Paths.get(filename).toFile.isFile ) {
      val evaluate = new YetAnotherArithmeticExpressionEvaluator()
      Using(Source.fromFile(filename)) { bufferedSource =>
        for (expr <- bufferedSource.getLines) {
          val answer = evaluate(expr)
          println(s"$expr = $answer")
        }
      }
    } else {
      println(s"Provide filename '$filename' either does not exist or is not a regular file.")
      System.exit(2)
    }
  }
}
