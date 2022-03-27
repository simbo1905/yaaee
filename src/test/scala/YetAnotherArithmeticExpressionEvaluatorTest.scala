class YetAnotherArithmeticExpressionEvaluatorTest extends org.scalatest.funsuite.AnyFunSuite {

  val evaluate = new YetAnotherArithmeticExpressionEvaluator()

  test("bad input")  {
    assertThrows[IllegalArgumentException] {
      evaluate("1 + 1 )")
    }
  }

  test("1 + 1") {
    assert(evaluate("1 + 1") === 2)
  }

  test("1 + 2 + 3") {
    assert(evaluate("1 + 2 + 3") === 6)
  }

  test( "2 * 2") {
    assert(evaluate("2 * 2") === 4)
  }

  test( "2 * 3 * 4") {
    assert(evaluate("2 * 3 * 4") === 24)
  }

  test( "1 + 2 * 3") {
    assert(evaluate("1 + 2 * 3") === 7)
  }

  test( "(1 + 2) * 3") {
    assert(evaluate("(1 + 2) * 3") === 9)
  }
}
