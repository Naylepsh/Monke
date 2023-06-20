import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class LexerSuite extends ScalaCheckSuite:
  test("Can get token from a simple string of symbols"):
    val input = "=+(){},;".toList
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
