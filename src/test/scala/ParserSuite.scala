import Parser.*

class ParserSuite extends munit.FunSuite:
  test("Parse simple chain of assignments"):
    val input = """let x = 5;
    |let y = 10;
    |let foobar = 838383;
    """.stripMargin
    val expected = Right(List(
      Statement.Let("x", Expression.Integer(5)),
      Statement.Let("y", Expression.Integer(10)),
      Statement.Let("foobar", Expression.Integer(838383))
    ))

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

  test("Can parse return statement"):
    val input = """return 5;
    |return 10;
    |return 993322;""".stripMargin
    val expected = Right(List(
      Statement.Return(Expression.Integer(5)),
      Statement.Return(Expression.Integer(10)),
      Statement.Return(Expression.Integer(993322))
    ))

    val tokens = Lexer.tokenize(input)
    val ast    = parse(tokens)

    assertEquals(ast.left.map(_.length), expected)
