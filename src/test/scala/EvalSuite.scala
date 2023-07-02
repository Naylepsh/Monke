class EvalSuite extends ParametrizedSuite:
  import ParametrizedSuite.TestParam
  import EvalSuite.*

  parametrizedTest(
    "Eval integer expression",
    List(
      TestParam(label = "just a value", input = ("42;", 42)),
      TestParam(label = "simple minus int", input = ("-42;", -42)),
      TestParam(label = "nested minuses", input = ("--42;", 42)),
      TestParam(label = "5", input = ("5", 5)),
      TestParam(label = "10", input = ("10", 10)),
      TestParam(label = "-5", input = ("-5", -5)),
      TestParam(label = "-10", input = ("-10", -10)),
      TestParam(
        label = "5 + 5 + 5 + 5 - 10",
        input = ("5 + 5 + 5 + 5 - 10", 10)
      ),
      TestParam(label = "2 * 2 * 2 * 2 * 2", input = ("2 * 2 * 2 * 2 * 2", 32)),
      TestParam(label = "-50 + 100 + -50", input = ("-50 + 100 + -50", 0)),
      TestParam(label = "5 * 2 + 10", input = ("5 * 2 + 10", 20)),
      TestParam(label = "5 + 2 * 10", input = ("5 + 2 * 10", 25)),
      TestParam(label = "20 + 2 * -10", input = ("20 + 2 * -10", 0)),
      TestParam(label = "50 / 2 * 2 + 10", input = ("50 / 2 * 2 + 10", 60)),
      TestParam(label = "2 * (5 + 10)", input = ("2 * (5 + 10)", 30)),
      TestParam(label = "3 * 3 * 3 + 10", input = ("3 * 3 * 3 + 10", 37)),
      TestParam(label = "3 * (3 * 3) + 10", input = ("3 * (3 * 3) + 10", 37)),
      TestParam(
        label = "(5 + 10 * 2 + 15 / 3) * 2 + -10",
        input = ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
      )
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
