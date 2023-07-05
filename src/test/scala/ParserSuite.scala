import Token.*
import Parser.*
import AST.*
import Node.given
import Program.*
import Show.*

class ParserSuite extends ParametrizedSuite:
  import ParametrizedSuite.TestParam

  test("Parse simple chain of assignments"):
    val input = """let x = 5;
    |let y = 10;
    |let foobar = 838383;
    """.stripMargin
    val expected = Right(Program(List(
      Statement.Let("x", Expression.IntegerLiteral(5)),
      Statement.Let("y", Expression.IntegerLiteral(10)),
      Statement.Let("foobar", Expression.IntegerLiteral(838383))
    )))

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  test("Fails to parse when let statement is not valid"):
    val input = """let x 5;
    |let = 10;
    |let 838383;
    """.stripMargin

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast.left.map(_.length), Left(3))

  test("Parse return statement"):
    val input = """return 5;
    |return 10;
    |return 993322;""".stripMargin
    val expected = Right(Program(List(
      Statement.Return(Expression.IntegerLiteral(5)),
      Statement.Return(Expression.IntegerLiteral(10)),
      Statement.Return(Expression.IntegerLiteral(993322))
    )))

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  test("Parse identifier expression statement"):
    val input = "foobar;"
    val expected =
      Right(Program(List(Statement.Expr(Expression.Identifier("foobar")))))

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  test("Parse integer expression statement"):
    val input = "42;"
    val expected =
      Right(Program(List(Statement.Expr(Expression.IntegerLiteral(42)))))

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  parametrizedTest(
    "Parse boolean expression statement",
    List(
      TestParam(
        label = "true",
        input = ("true;", Statement.Expr(Expression.BooleanLiteral(true)))
      ),
      TestParam(
        label = "false",
        input = ("false;", Statement.Expr(Expression.BooleanLiteral(false)))
      )
    )
  ): (input, expected) =>
    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, Right(Program(List(expected))))

  test("Parse string expression statement"):
    val input = """"foobar";"""
    val expected =
      Right(Program(List(Statement.Expr(Expression.StringLiteral("foobar")))))

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  test("Parse array expression"):
    val input = "[1, 2 * 2, 3 + 3];"
    val expected =
      Right(
        List(
          Expression.IntegerLiteral(1),
          Expression.InfixOperator(
            Expression.IntegerLiteral(2),
            Token.Asterisk,
            Expression.IntegerLiteral(2)
          ),
          Expression.InfixOperator(
            Expression.IntegerLiteral(3),
            Token.Plus,
            Expression.IntegerLiteral(3)
          )
        )
      )

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(
      ast.map(
        _.nodes.flatMap(_.asInstanceOf[Statement.Expr].expression.asInstanceOf[
          Expression.ArrayLiteral
        ].items.toList)
      ),
      expected
    )

  parametrizedTest(
    "Parse prefix expression",
    List(
      TestParam(
        label = "!42;",
        input = ("!42;", Token.Bang, Expression.IntegerLiteral(42))
      ),
      TestParam(
        label = "-42;",
        input = ("-42;", Token.Minus, Expression.IntegerLiteral(42))
      ),
      TestParam(
        label = "!true;",
        input = ("!true;", Token.Bang, Expression.BooleanLiteral(true))
      ),
      TestParam(
        label = "!false;",
        input = ("!false;", Token.Bang, Expression.BooleanLiteral(false))
      )
    )
  ): (input, expectedOp, expectedExpr) =>
    val expected =
      Right(Program(List(Statement.Expr(Expression.PrefixOperator(
        expectedOp,
        expectedExpr
      )))))
    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  test("Parse nested prefix expression"):
    val input = "!!42;"
    val expected = Right(Program(List(Statement.Expr(Expression.PrefixOperator(
      Token.Bang,
      Expression.PrefixOperator(Token.Bang, Expression.IntegerLiteral(42))
    )))))
    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  parametrizedTest(
    "Parse infix expression",
    List(
      TestParam(
        label = "5 + 5;",
        input = (
          "5 + 5;",
          Expression.IntegerLiteral(5),
          Token.Plus,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 - 5;",
        input = (
          "5 - 5;",
          Expression.IntegerLiteral(5),
          Token.Minus,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 * 5;",
        input = (
          "5 * 5;",
          Expression.IntegerLiteral(5),
          Token.Asterisk,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 / 5;",
        input = (
          "5 / 5;",
          Expression.IntegerLiteral(5),
          Token.Slash,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 > 5;",
        input = (
          "5 > 5;",
          Expression.IntegerLiteral(5),
          Token.GreaterThan,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 < 5;",
        input = (
          "5 < 5;",
          Expression.IntegerLiteral(5),
          Token.LesserThan,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 == 5;",
        input = (
          "5 == 5;",
          Expression.IntegerLiteral(5),
          Token.Equal,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "5 != 5;",
        input = (
          "5 != 5;",
          Expression.IntegerLiteral(5),
          Token.NotEqual,
          Expression.IntegerLiteral(5)
        )
      ),
      TestParam(
        label = "true == true",
        input = (
          "true == true",
          Expression.BooleanLiteral(true),
          Token.Equal,
          Expression.BooleanLiteral(true)
        )
      ),
      TestParam(
        label = "true != false",
        input = (
          "true != false",
          Expression.BooleanLiteral(true),
          Token.NotEqual,
          Expression.BooleanLiteral(false)
        )
      ),
      TestParam(
        label = "false == false",
        input = (
          "false == false",
          Expression.BooleanLiteral(false),
          Token.Equal,
          Expression.BooleanLiteral(false)
        )
      )
    )
  ): (input, expectedLeft, expectedOp, expectedRight) =>
    val expected = Right(Program(List(Statement.Expr(Expression.InfixOperator(
      expectedLeft,
      expectedOp,
      expectedRight
    )))))
    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  parametrizedTest(
    "Operator precendence",
    List(
      TestParam(label = "-a * b", input = ("-a * b", "((-a) * b)")),
      TestParam(
        label = "!-a",
        input = (
          "!-a",
          "(!(-a))",
        )
      ),
      TestParam(
        label = "a + b + c",
        input = (
          "a + b + c",
          "((a + b) + c)",
        )
      ),
      TestParam(
        label = "a + b - c",
        input = (
          "a + b - c",
          "((a + b) - c)",
        )
      ),
      TestParam(
        label = "a * b * c",
        input = (
          "a * b * c",
          "((a * b) * c)",
        )
      ),
      TestParam(
        label = "a * b / c",
        input = (
          "a * b / c",
          "((a * b) / c)",
        )
      ),
      TestParam(
        label = "a + b / c",
        input = (
          "a + b / c",
          "(a + (b / c))",
        )
      ),
      TestParam(
        label = "a + b * c + d / e - f",
        input = (
          "a + b * c + d / e - f",
          "(((a + (b * c)) + (d / e)) - f)",
        )
      ),
      TestParam(
        label = "3 + 4; -5 * 5",
        input = (
          "3 + 4; -5 * 5",
          """(3 + 4)
        |((-5) * 5)""".stripMargin,
        )
      ),
      TestParam(
        label = "5 > 4 == 3 < 4",
        input = (
          "5 > 4 == 3 < 4",
          "((5 > 4) == (3 < 4))",
        )
      ),
      TestParam(
        label = "5 < 4 != 3 > 4",
        input = (
          "5 < 4 != 3 > 4",
          "((5 < 4) != (3 > 4))",
        )
      ),
      TestParam(
        label = "3 + 4 * 5 == 3 * 1 + 4 * 5",
        input = (
          "3 + 4 * 5 == 3 * 1 + 4 * 5",
          "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        )
      ),
      TestParam(
        label = "3 + 4 * 5 == 3 * 1 + 4 * 5",
        input = (
          "3 + 4 * 5 == 3 * 1 + 4 * 5",
          "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        )
      ),
      TestParam(
        label = "3 > 5 == false",
        input = (
          "3 > 5 == false",
          "((3 > 5) == false)",
        )
      ),
      TestParam(
        label = "3 < 5 == true",
        input = (
          "3 < 5 == true",
          "((3 < 5) == true)",
        )
      ),
      TestParam(
        label = "1 + (2 + 3) + 4",
        input = (
          "1 + (2 + 3) + 4",
          "((1 + (2 + 3)) + 4)"
        )
      ),
      TestParam(
        label = "(5 + 5) * 2",
        input = (
          "(5 + 5) * 2",
          "((5 + 5) * 2)"
        )
      ),
      TestParam(
        label = "2 / (5 + 5)",
        input = (
          "2 / (5 + 5)",
          "(2 / (5 + 5))",
        )
      ),
      TestParam(
        label = "-(5 + 5)",
        input = (
          "-(5 + 5)",
          "(-(5 + 5))"
        )
      ),
      TestParam(
        label = "!(true == true)",
        input = (
          "!(true == true)",
          "(!(true == true))"
        )
      ),
      TestParam(
        label = "a + add(b * c) + d",
        input = (
          "a + add(b * c) + d",
          "((a + add((b * c))) + d)",
        )
      ),
      TestParam(
        label = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        input = (
          "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
          "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        )
      ),
      TestParam(
        label = "add(a + b + c * d / f + g)",
        input = (
          "add(a + b + c * d / f + g)",
          "add((((a + b) + ((c * d) / f)) + g))"
        )
      )
    )
  ): (input, expected) =>
    val tokens = Lexer.tokenize(input)

    val ast = Parser.parse(tokens)

    assertEquals(ast.map(_.show), Right(expected))

  test("Parse simple if expression"):
    val input = "if (x == y) { return true; }"
    val expected = Right(
      Program(
        List(
          Statement.Expr(
            Expression.If(
              Expression.InfixOperator(
                Expression.Identifier("x"),
                Token.Equal,
                Expression.Identifier("y")
              ),
              Statement.Block(
                List(Statement.Return(Expression.BooleanLiteral(true)))
              ),
              None
            )
          )
        )
      )
    )

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  test("Parse if expression with else clause"):
    val input = "if (x == y) { return true; } else { return false; }"
    val expected = Right(
      Program(
        List(
          Statement.Expr(
            Expression.If(
              Expression.InfixOperator(
                Expression.Identifier("x"),
                Token.Equal,
                Expression.Identifier("y")
              ),
              Statement.Block(
                List(Statement.Return(Expression.BooleanLiteral(true)))
              ),
              Some(Statement.Block(
                List(Statement.Return(Expression.BooleanLiteral(false)))
              ))
            )
          )
        )
      )
    )

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast, expected)

  parametrizedTest(
    "Parse function expression",
    List[TestParam[(String, List[Expression.Identifier])]](
      TestParam(label = "0 args", input = ("fn() { return 42; }", List.empty)),
      TestParam(
        label = "1 arg",
        input = ("fn(x) { return 42; }", List(Expression.Identifier("x")))
      ),
      TestParam(
        label = "2 args",
        input = (
          "fn(x, y) { return 42; }",
          List(Expression.Identifier("x"), Expression.Identifier("y"))
        )
      )
    )
  ): (input, expectedParameters) =>
    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(
      ast,
      Right(
        Program(
          List(
            Statement.Expr(
              Expression.Func(
                expectedParameters,
                Statement.Block(
                  List(
                    Statement.Return(Expression.IntegerLiteral(42))
                  )
                )
              )
            )
          )
        )
      )
    )

  parametrizedTest(
    "Parse call expression",
    List(
      TestParam(
        label = "0 args",
        input = ("sum()", Expression.Identifier("sum"), List.empty)
      ),
      TestParam(
        label = "1 args",
        input = (
          "sum(x)",
          Expression.Identifier("sum"),
          List(Expression.Identifier("x"))
        )
      ),
      TestParam(
        label = "2 args",
        input = (
          "sum(x, y)",
          Expression.Identifier("sum"),
          List(Expression.Identifier("x"), Expression.Identifier("y"))
        )
      ),
      TestParam(
        label = "2 args where one is expression",
        input = (
          "sum(x, y + z)",
          Expression.Identifier("sum"),
          List(
            Expression.Identifier("x"),
            Expression.InfixOperator(
              Expression.Identifier("y"),
              Token.Plus,
              Expression.Identifier("z")
            )
          )
        )
      ),
      TestParam(
        label = "immediately invoked function",
        input = (
          "fn(x) { return x; }(42)",
          Expression.Func(
            List(Expression.Identifier("x")),
            Statement.Block(List(Statement.Return(Expression.Identifier("x"))))
          ),
          List(Expression.IntegerLiteral(42))
        )
      )
    )
  ): (input, expectedFunc, expectedArgs) =>
    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(
      ast,
      Right(Program(List(Statement.Expr(Expression.Call(
        expectedFunc,
        expectedArgs
      )))))
    )
