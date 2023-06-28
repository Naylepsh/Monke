import Token.*
import Parser.*
import AST.*
import Node.given
import Program.*
import Show.*

class ParserSuite extends munit.FunSuite:
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

  test("Parse boolean expression statement"):
    val testCases = List(
      ("true;", Statement.Expr(Expression.BooleanLiteral(true))),
      ("false;", Statement.Expr(Expression.BooleanLiteral(false)))
    )
    testCases.foreach: (input, expected) =>
      val tokens = Lexer.tokenize(input)
      val ast    = parse(tokens)

      assertEquals(ast, Right(Program(List(expected))))

  test("Parse prefix expression"):
    val testCases = List(
      ("!42;", Token.Bang, Expression.IntegerLiteral(42)),
      ("-42;", Token.Minus, Expression.IntegerLiteral(42)),
      ("!true;", Token.Bang, Expression.BooleanLiteral(true)),
      ("!false;", Token.Bang, Expression.BooleanLiteral(false))
    )
    testCases.foreach: (input, expectedOp, expectedExpr) =>
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

  test("Parse infix expression"):
    val testCases = List(
      (
        "5 + 5;",
        Expression.IntegerLiteral(5),
        Token.Plus,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 - 5;",
        Expression.IntegerLiteral(5),
        Token.Minus,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 * 5;",
        Expression.IntegerLiteral(5),
        Token.Asterisk,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 / 5;",
        Expression.IntegerLiteral(5),
        Token.Slash,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 > 5;",
        Expression.IntegerLiteral(5),
        Token.GreaterThan,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 < 5;",
        Expression.IntegerLiteral(5),
        Token.LesserThan,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 == 5;",
        Expression.IntegerLiteral(5),
        Token.Equal,
        Expression.IntegerLiteral(5)
      ),
      (
        "5 != 5;",
        Expression.IntegerLiteral(5),
        Token.NotEqual,
        Expression.IntegerLiteral(5)
      ),
      (
        "true == true",
        Expression.BooleanLiteral(true),
        Token.Equal,
        Expression.BooleanLiteral(true)
      ),
      (
        "true != false",
        Expression.BooleanLiteral(true),
        Token.NotEqual,
        Expression.BooleanLiteral(false)
      ),
      (
        "false == false",
        Expression.BooleanLiteral(false),
        Token.Equal,
        Expression.BooleanLiteral(false)
      )
    )

    testCases.foreach: (input, expectedLeft, expectedOp, expectedRight) =>
      val expected = Right(Program(List(Statement.Expr(Expression.InfixOperator(
        expectedLeft,
        expectedOp,
        expectedRight
      )))))
      val tokens = Lexer.tokenize(input)
      val ast    = parse(tokens)

      assertEquals(ast, expected)

  test("Operator precendence"):
    val testCases = List(
      ("-a * b", "((-a) * b)"),
      (
        "!-a",
        "(!(-a))",
      ),
      (
        "a + b + c",
        "((a + b) + c)",
      ),
      (
        "a + b - c",
        "((a + b) - c)",
      ),
      (
        "a * b * c",
        "((a * b) * c)",
      ),
      (
        "a * b / c",
        "((a * b) / c)",
      ),
      (
        "a + b / c",
        "(a + (b / c))",
      ),
      (
        "a + b * c + d / e - f",
        "(((a + (b * c)) + (d / e)) - f)",
      ),
      (
        "3 + 4; -5 * 5",
        """(3 + 4)
        |((-5) * 5)""".stripMargin,
      ),
      (
        "5 > 4 == 3 < 4",
        "((5 > 4) == (3 < 4))",
      ),
      (
        "5 < 4 != 3 > 4",
        "((5 < 4) != (3 > 4))",
      ),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
      ),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
      ),
      (
        "3 > 5 == false",
        "((3 > 5) == false)",
      ),
      (
        "3 < 5 == true",
        "((3 < 5) == true)",
      ),
      (
        "1 + (2 + 3) + 4",
        "((1 + (2 + 3)) + 4)"
      ),
      (
        "(5 + 5) * 2",
        "((5 + 5) * 2)"
      ),
      (
        "2 / (5 + 5)",
        "(2 / (5 + 5))",
      ),
      (
        "-(5 + 5)",
        "(-(5 + 5))"
      ),
      (
        "!(true == true)",
        "(!(true == true))"
      )
    )
    testCases.foreach: (input, expected) =>
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
