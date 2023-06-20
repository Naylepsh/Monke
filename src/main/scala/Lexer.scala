enum Token:
  case Illegal                   extends Token
  case EOF                       extends Token
  case Identifier(value: String) extends Token
  case Integer(value: String)    extends Token
  case Assign                    extends Token
  case Plus                      extends Token
  case Comma                     extends Token
  case Semicolon                 extends Token
  case LeftParen                 extends Token
  case RightParen                extends Token
  case LeftBrace                 extends Token
  case RightBrace                extends Token
  case Func                      extends Token
  case Let                       extends Token

object Lexer:
  def nextToken(input: List[Char]): (Token, List[Char]) =
    input match
      case Nil         => Token.EOF        -> input
      case '=' :: rest => Token.Assign     -> rest
      case '+' :: rest => Token.Plus       -> rest
      case ',' :: rest => Token.Comma      -> rest
      case ';' :: rest => Token.Semicolon  -> rest
      case '(' :: rest => Token.LeftParen  -> rest
      case ')' :: rest => Token.RightParen -> rest
      case '{' :: rest => Token.LeftBrace  -> rest
      case '}' :: rest => Token.RightBrace -> rest
      case char :: rest if isInteger(char) =>
        val (int, leftoverInput) = peekInteger(char :: rest)
        Token.Integer(int) -> leftoverInput
      case char :: rest if isIdentifier(char) =>
        val (identifier, leftoverChars) = peekIdentifier(char :: rest)
        val token = identifier match
          case "let"  => Token.Let
          case "func" => Token.Func
          case other  => Token.Identifier(other)
        token -> leftoverChars
      case _ => Token.Illegal -> input

  private def isInteger(char: Char)    = char.isDigit
  private def isIdentifier(char: Char) = char.isLetter || char == '_'

  private val peekInteger    = peekText(isInteger)
  private val peekIdentifier = peekText(isIdentifier)

  def peekText(predicate: Char => Boolean)(input: List[Char])
      : (String, List[Char]) =
    @annotation.tailrec
    def peek(
        input: List[Char],
        accValue: String
    ): (String, List[Char]) =
      input match
        case char :: rest if predicate(char) => peek(rest, char + accValue)
        case rest                            => accValue.reverse -> rest

    peek(input, "")

  def tokenize(input: String): List[Token] =
    @annotation.tailrec
    def doTokenize(input: List[Char], tokens: List[Token]): List[Token] =
      nextToken(input) match
        case (Token.EOF, _) => Token.EOF :: tokens
        case (token, Nil)   => Token.EOF :: token :: tokens
        case (token, rest)  => doTokenize(rest, token :: tokens)

    doTokenize(input.toList, List()).reverse
