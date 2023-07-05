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
      TestParam(label = "false", input = ("false;", false)),
      TestParam(label = "1 < 2", input = ("1 < 2", true)),
      TestParam(label = "1 > 2", input = ("1 > 2", false)),
      TestParam(label = "1 < 1", input = ("1 < 1", false)),
      TestParam(label = "1 > 1", input = ("1 > 1", false)),
      TestParam(label = "1 == 1", input = ("1 == 1", true)),
      TestParam(label = "1 != 1", input = ("1 != 1", false)),
      TestParam(label = "1 == 2", input = ("1 == 2", false)),
      TestParam(label = "1 != 2", input = ("1 != 2", true)),
      TestParam(label = "true == true", input = ("true == true", true)),
      TestParam(label = "false == false", input = ("false == false", true)),
      TestParam(label = "true == false", input = ("true == false", false)),
      TestParam(label = "true != false", input = ("true != false", true)),
      TestParam(label = "false != true", input = ("false != true", true)),
      TestParam(label = "(1 < 2) == true", input = ("(1 < 2) == true", true)),
      TestParam(
        label = "(1 < 2) == false",
        input = ("(1 < 2) == false", false)
      ),
      TestParam(label = "(1 > 2) == true", input = ("(1 > 2) == true", false)),
      TestParam(label = "(1 > 2) == false", input = ("(1 > 2) == false", true))
    )
  ): (input, expectedValue) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.BooleanLiteral(expectedValue)))

  parametrizedTest(
    "Eval string expression",
    List(
      TestParam(label = """"foo"""", input = (""""foo"""", "foo")),
      TestParam(
        label = """"foo" + "bar"""",
        input = (""""foo" + "bar"""", "foobar")
      )
    )
  ): (input, expectedValue) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.StringLiteral(expectedValue)))

  test("Eval array literal"):
    val input = "[1, 2 * 2, 3 + 3]"
    val expected = Right(
      List(
        MonkeyObject.IntegerLiteral(1),
        MonkeyObject.IntegerLiteral(4),
        MonkeyObject.IntegerLiteral(6)
      )
    )

    val result = eval(input)

    assertEquals(
      result.map(_.asInstanceOf[MonkeyObject.ArrayLiteral].value.toList),
      expected
    )

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

  parametrizedTest(
    "Eval if expression",
    List(
      TestParam(
        label = "if (true) { 10 }",
        input = ("if (true) { 10 }", Some(10))
      ),
      TestParam(
        label = "if (false) { 10 }",
        input = ("if (false) { 10 }", None)
      ),
      TestParam(label = "if (1) { 10 }", input = ("if (1) { 10 }", Some(10))),
      TestParam(
        label = "if (1 < 2) { 10 }",
        input = ("if (1 < 2) { 10 }", Some(10))
      ),
      TestParam(
        label = "if (1 > 2) { 10 }",
        input = ("if (1 > 2) { 10 }", None)
      ),
      TestParam(
        label = "if (1 > 2) { 10 } else { 20 }",
        input = ("if (1 > 2) { 10 } else { 20 }", Some(20))
      ),
      TestParam(
        label = "if (1 < 2) { 10 } else { 20 }",
        input = ("if (1 < 2) { 10 } else { 20 }", Some(10))
      )
    )
  ): (input, expectedValue) =>
    val expectedMonkey = expectedValue
      .map(MonkeyObject.IntegerLiteral(_))
      .getOrElse(MonkeyObject.Null)

    val result = eval(input)

    assertEquals(result, Right(expectedMonkey))

  parametrizedTest(
    "Eval return statement",
    List(
      TestParam(label = "return 10;", input = ("return 10;", 10)),
      TestParam(label = "return 10; 9;", input = ("return 10; 9;", 10)),
      TestParam(label = "return 2 * 5; 9;", input = ("return 2 * 5; 9;", 10)),
      TestParam(
        label = "9; return 2 * 5; 9;",
        input = ("9; return 2 * 5; 9;", 10)
      ),
      TestParam(
        label = "Return in nested blocks",
        input = (
          s"""
          |if (10 > 1) {
          |   if (10 > 1) {
          |     return 10;
          |   }
          |   return 1;
          |}""".stripMargin,
          10
        )
      )
    )
  ): (input, expected) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.IntegerLiteral(expected)))

  parametrizedTest(
    "Eval invalid syntax code",
    List(
      TestParam(label = "Int + Boolean", input = ("5 + true;")),
      TestParam(label = "Int + Boolean", input = ("5 + true; 5;")),
      TestParam(label = "-Boolean", input = ("-true")),
      TestParam(label = "Boolean + Boolean", input = ("true + false;")),
      TestParam(label = "Boolean + Boolean", input = ("5; true + false; 5")),
      TestParam(
        label = "Boolean + Boolean",
        input = ("if (10 > 1) { true + false; }")
      )
    )
  ): (input) =>
    val result = eval(input)

    assertEquals(
      result.left.map(_.isInstanceOf[Eval.EvalutationError.InvalidSyntax]),
      Left(true)
    )

  test("Eval undefined variable"):
    val input = "foobar;"

    val result = eval(input)

    assertEquals(
      result,
      Left(Eval.EvalutationError.UndefinedIdentifier("foobar"))
    )

  parametrizedTest(
    "Eval let statement",
    List(
      TestParam(label = "let a = 5; a;", input = ("let a = 5; a;", 5)),
      TestParam(label = "let a = 5 * 5; a;", input = ("let a = 5 * 5; a;", 25)),
      TestParam(
        label = "let a = 5; let b = a; b;",
        input = ("let a = 5; let b = a; b;", 5)
      ),
      TestParam(
        label = "let a = 5; let b = a; let c = a + b + 5; c;",
        input = ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
      )
    )
  ): (input, expected) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.IntegerLiteral(expected)))

  test("Eval function definition"):
    val input = "fn(x) { return x + 1; }"

    val result = eval(input)

    result match
      case Right(MonkeyObject.FunctionLiteral(params, body, _)) =>
        assertEquals(params.length, 1)
        assertEquals(
          params.head,
          AST.Expression.Identifier("x").asInstanceOf[AST.Expression.Identifier]
        )
      case _ => assert(false, s"$result is not an expected func literal")

  parametrizedTest(
    "Eval function application",
    List(
      TestParam(
        label = "let identity = fn(x) { x; }; identity(5);",
        input = ("let identity = fn(x) { x; }; identity(5);", 5)
      ),
      TestParam(
        label = "let identity = fn(x) { return x; }; identity(5);",
        input = ("let identity = fn(x) { return x; }; identity(5);", 5)
      ),
      TestParam(
        label = "let double = fn(x) { x * 2; }; double(5);",
        input = ("let double = fn(x) { x * 2; }; double(5);", 10)
      ),
      TestParam(
        label = "let add = fn(x, y) { x + y; }; add(5, 5);",
        input = ("let add = fn(x, y) { x + y; }; add(5, 5);", 10)
      ),
      TestParam(
        label = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
        input = ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20)
      ),
      TestParam(label = "fn(x) { x; }(5)", input = ("fn(x) { x; }(5)", 5))
    )
  ): (input, expected) =>
    val result = eval(input)

    assertEquals(result, Right(MonkeyObject.IntegerLiteral(expected)))

object EvalSuite:
  def eval(input: String) =
    Parser
      .parse(Lexer.tokenize(input))
      .map: tokens =>
        Eval
          .eval(tokens, Environment.empty)
          .map: (result, _) =>
            result
      // Theoretically unsafe, but in this test suite we don't care about parsing failures
      .toOption
      .get
