class EvalSuite extends munit.FunSuite:
  import EvalSuite.*

  test("Eval integer literal"):
    val input    = "42;"
    val expected = MonkeyObject.IntegerLiteral(42)

    val result = eval(input)

    assertEquals(result, Right(expected))

  test("Eval boolean literal"):
    val testCases = List(
      ("true;", MonkeyObject.BooleanLiteral(true)),
      ("false;", MonkeyObject.BooleanLiteral(false))
    )
    testCases.foreach: (input, expected) =>
      val result = eval(input)

      assertEquals(result, Right(expected))

  test("Eval empty program"):
    val testCases = List("", ";", ";;")
    testCases.foreach: input =>
      val result = eval(input)

      assertEquals(result, Right(MonkeyObject.Null))

object EvalSuite:
  def eval(input: String) =
    Parser
      .parse(Lexer.tokenize(input))
      .map(Eval.eval)
