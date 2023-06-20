import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary

class LexerSuite extends ScalaCheckSuite:
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

  property("Can tokenize a program"):
    import LexerSuite.{ variableNameGen, isValidVariableName }

    forAll(
      variableNameGen,
      variableNameGen,
      variableNameGen
    ) {
      (var1: String, var2: String, funcName: String) =>
        (
          isValidVariableName(var1)
            && isValidVariableName(var2)
            && isValidVariableName(funcName)
        ) ==> {
          val input = s"""let $var1 = 5;
            |let $var2 = 10;
            |
            |let $funcName = fn(x, y) {
            |   x + y;
            |};
            |
            |let result = $funcName($var1, $var2);
            """.stripMargin
          val expected = List(
            // let var1 = 5;
            Token.Let,
            Token.Identifier(var1),
            Token.Assign,
            Token.Integer("5"),
            Token.Semicolon,
            // let var2 = 10;
            Token.Let,
            Token.Identifier(var2),
            Token.Assign,
            Token.Integer("10"),
            Token.Semicolon,
            // let funcName = fn(x, y) {
            //  x + y;
            // };
            Token.Let,
            Token.Identifier(funcName),
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
            // let result = funcName(var1, var2);
            Token.Let,
            Token.Identifier("result"),
            Token.Assign,
            Token.Identifier(funcName),
            Token.LeftParen,
            Token.Identifier(var1),
            Token.Comma,
            Token.Identifier(var2),
            Token.RightParen,
            Token.Semicolon,
            Token.EOF
          )
          val tokens = Lexer.tokenize(input)

          tokens == expected
        }
    }

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

object LexerSuite:
  val validChars = (('A' to 'Z') ++ ('a' to 'z') ++ Seq('_')).toSet
  val variableNameGen =
    for
      n    <- Gen.choose(1, 16)
      list <- Gen.listOfN(n, Gen.oneOf(validChars))
    yield list.mkString("")

  def isValidVariableName(str: String): Boolean =
    str.filter(validChars.contains) == str && str.length > 0
