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
    import LexerSuite.{ variableNameGen, isValidVariableName, VariableName }

    forAll(variableNameGen, variableNameGen, variableNameGen) {
      (a: VariableName, b: VariableName, funcName: VariableName) =>
        (isValidVariableName(a.value)
          && isValidVariableName(b.value)
          && isValidVariableName(funcName.value)) ==> {
          val input = s"""let ${a.value} = 5;
            |let ${b.value} = 10;
            |
            |let ${funcName.value} = fn(x, y) {
            |   x + y;
            |};
            |
            |let result = add(${a.value}, ${b.value});
            """.stripMargin
          val expected = List(
            // let five = 5;
            Token.Let,
            Token.Identifier(a.value),
            Token.Assign,
            Token.Integer("5"),
            Token.Semicolon,
            // let ten = 10;
            Token.Let,
            Token.Identifier(b.value),
            Token.Assign,
            Token.Integer("10"),
            Token.Semicolon,
            // let add = fn(x, y) {
            //  x + y;
            // };
            Token.Let,
            Token.Identifier(funcName.value),
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
            Token.Identifier(funcName.value),
            Token.LeftParen,
            Token.Identifier(a.value),
            Token.Comma,
            Token.Identifier(b.value),
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
  case class VariableName(value: String)

  def isValidVariableName(str: String): Boolean =
    str.filter(_.isLetter) == str && !str.isEmpty()
  val variableNameGen =
    Arbitrary.arbitrary[String].suchThat(isValidVariableName).map(VariableName(_))
