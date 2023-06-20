object Lexer:
  def nextToken(input: List[Char]): (Token, List[Char]) =
    input match
      case Nil                               => Token.EOF        -> input
      case '=' :: rest                       => Token.Assign     -> rest
      case '+' :: rest                       => Token.Plus       -> rest
      case ',' :: rest                       => Token.Comma      -> rest
      case ';' :: rest                       => Token.Semicolon  -> rest
      case '(' :: rest                       => Token.LeftParen  -> rest
      case ')' :: rest                       => Token.RightParen -> rest
      case '{' :: rest                       => Token.LeftBrace  -> rest
      case '}' :: rest                       => Token.RightBrace -> rest
      case char :: rest if char.isWhitespace => nextToken(eatWhitespace(rest))
      case all @ char :: _ if char.isDigit =>
        val (int, leftoverInput) = peekInteger(all)
        Token.Integer(int) -> leftoverInput
      case all @ char :: _ if identifierChars.contains(char) =>
        val (identifier, leftoverChars) = peekIdentifier(all)
        val token = identifier match
          case "let" => Token.Let
          case "fn"  => Token.Func
          case other => Token.Identifier(other)
        token -> leftoverChars
      case _ => Token.Illegal -> input

  private def eatWhitespace(input: List[Char]): List[Char] =
    input match
      case Nil                               => Nil
      case char :: rest if char.isWhitespace => eatWhitespace(rest)
      case rest                              => rest

  private val identifierChars = (('a' to 'z') ++ ('A' to 'Z') ++ Seq('_')).toSet
  private val peekInteger     = peekText(_.isDigit)
  private val peekIdentifier  = peekText(identifierChars.contains)

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
