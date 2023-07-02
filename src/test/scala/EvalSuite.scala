class EvalSuite extends ParametrizedSuite:
  import ParametrizedSuite.TestParam
  import EvalSuite.*

  test("Eval integer literal"):
    val input    = "42;"
    val expected = MonkeyObject.IntegerLiteral(42)

    val result = eval(input)

    assertEquals(result, Right(expected))

  parametrizedTest(
    "Eval boolean literal",
    List(
      TestParam(
        label = "true",
        input = ("true;", MonkeyObject.BooleanLiteral(true))
      ),
      TestParam(
        label = "false",
        input = ("false;", MonkeyObject.BooleanLiteral(false))
      )
    )
  ): (input, expected) =>
    val result = eval(input)

    assertEquals(result, Right(expected))

  parametrizedTest(
    "Eval empty program",
    List(
      TestParam(label = "", input = ""),
      TestParam(label = ";", input = ";"),
      TestParam(label = ";;", input = ";;")
    )
  ): (input) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.Null))

object EvalSuite:
  def eval(input: String) =
    Parser
      .parse(Lexer.tokenize(input))
      .map(Eval.eval)
