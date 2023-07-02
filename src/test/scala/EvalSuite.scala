class EvalSuite extends ParametrizedSuite:
  import ParametrizedSuite.TestParam
  import EvalSuite.*

  parametrizedTest(
    "Eval integer expression",
    List(
      TestParam(label = "just a value", input = ("42;", 42)),
      TestParam(label = "simple minus int", input = ("-42;", -42)),
      TestParam(label = "nested minuses", input = ("--42;", 42))
    )
  ): (input, expectedValue) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.IntegerLiteral(expectedValue)))

  parametrizedTest(
    "Eval boolean expression",
    List(
      TestParam(label = "true", input = ("true;", true)),
      TestParam(label = "false", input = ("false;", false))
    )
  ): (input, expectedValue) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.BooleanLiteral(expectedValue)))

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

  parametrizedTest(
    "Eval bang prefix operator on valid expression",
    List(
      TestParam(label = "!true", input = ("!true;", false)),
      TestParam(label = "!false", input = ("!false;", true)),
      TestParam(label = "!!false", input = ("!!false;", false)),
      TestParam(label = "!!true", input = ("!!true;", true)),
      TestParam(label = "!5", input = ("!5;", false)),
      TestParam(label = "!!5", input = ("!!5;", true)),
      TestParam(label = "!0", input = ("!0;", true))
    )
  ): (input, expectedValue) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.BooleanLiteral(expectedValue)))

object EvalSuite:
  def eval(input: String) =
    Parser
      .parse(Lexer.tokenize(input))
      .map(Eval.eval)
      // Theoretically unsafe, but in this test suite we don't care about parsing failures
      .toOption
      .get
