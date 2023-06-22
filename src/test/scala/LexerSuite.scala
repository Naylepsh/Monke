class LexerSuite extends munit.FunSuite:
  test("Can get token from a simple string of symbols"):
    val input    = "=+(){},;".toList
    val expected = (Token.Assign, input.tail)

    val result = Lexer.nextToken(input)

    assertEquals(result, expected)

  test("Can tokenize simple string of symbols"):
    val input = "=+(){},;"
    val expected = List(
      Token.Assign,
      Token.Plus,
      Token.LeftParen,
      Token.RightParen,
      Token.LeftBrace,
      Token.RightBrace,
      Token.Comma,
      Token.Semicolon,
      Token.EOF
    )

    val tokens = Lexer.tokenize(input)

    assertEquals(tokens, expected)

  test("Can tokenize a program"):
    val input = """let five = 5;
    |let ten = 10;
    |
    |let add = fn(x, y) {
    |   x + y;
    |};
    |
    |let result = add(five, ten);
    """.stripMargin
    val expected = List(
      // let five = 5;
      Token.Let,
      Token.Identifier("five"),
      Token.Assign,
      Token.Integer("5"),
      Token.Semicolon,
      // let ten = 10;
      Token.Let,
      Token.Identifier("ten"),
      Token.Assign,
      Token.Integer("10"),
      Token.Semicolon,
      // let add = fn(x, y) {
      //  x + y;
      // };
      Token.Let,
      Token.Identifier("add"),
      Token.Assign,
      Token.Func,
      Token.LeftParen,
      Token.Identifier("x"),
      Token.Comma,
      Token.Identifier("y"),
      Token.RightParen,
      Token.LeftBrace,
      Token.Identifier("x"),
      Token.Plus,
      Token.Identifier("y"),
      Token.Semicolon,
      Token.RightBrace,
      Token.Semicolon,
      // let result = add(five, ten);
      Token.Let,
      Token.Identifier("result"),
      Token.Assign,
      Token.Identifier("add"),
      Token.LeftParen,
      Token.Identifier("five"),
      Token.Comma,
      Token.Identifier("ten"),
      Token.RightParen,
      Token.Semicolon,
      Token.EOF
    )

    val tokens = Lexer.tokenize(input)

    assertEquals(tokens, expected)

  test("Can tokenize program2"):
    val input = """!-/*5;
    |5 < 10 > 5;
    |
    |if (5 < 10) {
    |  return true;
    |} else {
    |  return false;
    |}
    |
    |10 == 10;
    |10 != 9;""".stripMargin
    val expected = List(
      Token.Bang,
      Token.Minus,
      Token.Slash,
      Token.Asterisk,
      Token.Integer("5"),
      Token.Semicolon,
      Token.Integer("5"),
      Token.LesserThan,
      Token.Integer("10"),
      Token.GreaterThan,
      Token.Integer("5"),
      Token.Semicolon,
      Token.If,
      Token.LeftParen,
      Token.Integer("5"),
      Token.LesserThan,
      Token.Integer("10"),
      Token.RightParen,
      Token.LeftBrace,
      Token.Return,
      Token.True,
      Token.Semicolon,
      Token.RightBrace,
      Token.Else,
      Token.LeftBrace,
      Token.Return,
      Token.False,
      Token.Semicolon,
      Token.RightBrace,
      Token.Integer("10"),
      Token.Equal,
      Token.Integer("10"),
      Token.Semicolon,
      Token.Integer("10"),
      Token.NotEqual,
      Token.Integer("9"),
      Token.Semicolon,
      Token.EOF
    )

    val tokens = Lexer.tokenize(input)

    assertEquals(tokens, expected)
