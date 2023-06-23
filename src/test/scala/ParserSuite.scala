import Parser.*

class ParserSuite extends munit.FunSuite:
  test("Can parse simple chain of assignments"):
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
