
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator._

/**
 * A classical Scala Parse Combinator implementation of a simple expression evaluator.
 * It can parse a simple integer arithmetic expression to give an integer result.
 */
class YetAnotherArithmeticExpressionEvaluator extends JavaTokenParsers
{
  def apply(expression: String): Int = parseAll(expr, expression) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new IllegalArgumentException(s"could not parse '$expression' with error $msg")
  }

  /**
   * An expression is the sum() function applied to a sequence of matches that is at least one term followed by
   * repeated "+term" or "-term"
   * @return the adds/subtracts of the integers returned by the terms
   */
  def expr: Parser[Int] = term~rep("+"~term | "-"~term) ^^ sum

  /**
   * A term is the product() function applied to a sequence of matches that is at least one number followed by a
   * repeated "*term" or "/term".
   * @return the multiply/divides of the numbers
   */
  def term: Parser[Int] = number~rep("*"~number | "/"~number) ^^ product

  /**
   * A number is a number else an expression between braces.
   * @return the number literal else the valuation of the expression within braces
   */
  def number: Parser[Int] = wholeNumber ^^ (_.toInt) | "("~>expr<~")"

  /**
   * Performs addition or subtraction over a parsed token stream.
   * @param in The parsed token sequence returned by term~rep("+"~term | "-"~term)
   * @return
   */
  def sum( in: ~[Int, List[~[String, Int]]]): Int = in match {
    case n0~sgnNumberList => sgnNumberList.foldLeft(n0) {
      case (z, "+"~n) => z+n
      case (z, "-"~n) => z-n
    }
  }

  /**
   * Performs multiplication or division over a parsed token stream.
   * @param in The parsed token sequence returned by number~rep("*"~number | "/"~number)
   * @return
   */
  def product( in: ~[Int, List[~[String, Int]]]): Int = in match {
    case n0~opNumberList => opNumberList.foldLeft(n0) {
      case (z, "*"~n) => z*n
      case(z, "/"~n) => z/n
    }
  }
}

import scala.io.Source
import scala.util.Using

/**
 * A main class that validates arguments, exits with error codes, and that chooses to continue in the case of a bad
 * data within the provided file logging the error to standard out while returning an error code.
 */
object YetAnotherArithmeticExpressionEvaluator extends App {
  var exitCode = 0
  if (args.length == 0) {
    println("Please provide the name of a file with one expression per line.")
    exitCode = 1
  } else {
    val filename = args(0)
    if( Files.exists(Paths.get(filename)) && Paths.get(filename).toFile.isFile ) {
      val evaluate = new YetAnotherArithmeticExpressionEvaluator()
      Using(Source.fromFile(filename)) { bufferedSource =>
        var line = 1
        for (expr <- bufferedSource.getLines) {
          Try {
            val answer = evaluate(expr)
            println(s"$expr = $answer")
            line += line
          } match {
            case Failure(exception) =>
              exitCode = 3
              System.err.println(s"line $line - $exception.toString")
            case Success(_) => // all good
          }
        }
      }
    } else {
      println(s"Provided filename '$filename' either does not exist or is not a regular file.")
      exitCode = 2
    }
    System.exit(exitCode)
  }
}
