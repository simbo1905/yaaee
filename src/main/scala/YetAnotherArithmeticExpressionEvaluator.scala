
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
    case _ => throw new AssertionError("Code not reachable but needed to silence compiler warning.")
  }

  /**
   * A expr is a parser that returns an Int that applies the sum() function to a sequence of least one term
   * followed by repeated "+ term" or "- term". This level of 'expr' is lowest precedence as it requires all
   * terms to be evaluated to Ints to pass into sum().
   */
  def expr: Parser[Int] = term~rep("+"~term | "-"~term) ^^ sum

  /**
   * A term is a parser that returns an Int that applies the product() function to a sequence of at least one
   * numberOrBracedExpr followed by repeated "* numberOrBracedExpr" or "/ numberOrBracedExpr". This level has
   * lower precedence than numberOrBracedExpr as it requires all of them to be evaulated into Ints to pass into
   * product().
   */
  def term: Parser[Int] = numberOrBracedExpr~rep("*"~numberOrBracedExpr | "/"~numberOrBracedExpr) ^^ product

  /**
   * A numberOrBracedExpr is a "wholeNumber.toInt" else an expr between braces "( expr )". The wholeNumbers.toInt form
   * the leaf nodes of the logical AST. These are propagated up call chain. This statement does a logical recursion into
   * any '( expr )' which has highest precedence as it must be evaluated to an Int for this parser to return an Int.
   */
  def numberOrBracedExpr: Parser[Int] = wholeNumber ^^ (_.toInt) | "("~>expr<~")"

  /**
   * Performs addition and subtraction of the list of "+|- Int" that terms have been evaluated to.
   * @param in The structure returned by evaluating terms as Ints in term~rep("+"~term|"-"~term)
   * @return The result of the additions and subtractions.
   */
  def sum( in: ~[Int, List[~[String, Int]]]): Int = in match {
    case i~sgnNumberList => sgnNumberList.foldLeft(i) {
      case (i, "+"~n) => i+n
      case (i, "-"~n) => i-n
      case _ => throw new AssertionError("Code not reachable but needed to silence compiler warning.")
    }
  }

  /**
   * Performs multiplication or division of the list of "*|/ Int" that numbers have been evaluated to. 
   * @param in The structure returned by evaluating numberOrBraced as Ints in numberOrBracedExpr~rep("*"~numberOrBracedExpr|"/"~numberOrBracedExpr)
   * @return The result of the multiplications and divisions.
   */
  def product( in: ~[Int, List[~[String, Int]]]): Int = in match {
    case i~sgnNumberList => sgnNumberList.foldLeft(i) {
      case (i, "*"~n) => i*n
      case(i, "/"~n) => i/n
      case _ => throw new AssertionError("Code not reachable but needed to silence compiler warning.")
    }
  }
}

import scala.io.Source
import scala.util.Using

/**
 * A main class that validates arguments, exits with error codes for bad run arguments, and that chooses to continue
 * processing in the face of bad expression within the file provided. It will print a warning message on stderr for bad
 * expressions naming the line number.
 */
object YetAnotherArithmeticExpressionEvaluator  {
  def main(args: Array[String]): Unit ={
    if (args.length == 0) {
      System.err.println("Please provide the name of a file with one expression per line.")
      System.exit(1)
    } else {
      val filename = args(0)
      if( Files.exists(Paths.get(filename)) && Paths.get(filename).toFile.isFile ) {
        val evaluate = new YetAnotherArithmeticExpressionEvaluator()
        Using(Source.fromFile(filename)) { bufferedSource =>
          var line = 1
          for (expr <- bufferedSource.getLines()) {
            Try {
              val answer = evaluate(expr)
              println(s"$expr = $answer")
              line += line
            } match {
              case Failure(exception) =>
                System.err.println(s"[WARN] line $line - ${exception}")
              case Success(_) => // all good
            }
          }
        }
      } else {
        System.err.println(s"Provided filename '$filename' either does not exist or is not a regular file.")
        System.exit(2)
      }
    }
  }


}
