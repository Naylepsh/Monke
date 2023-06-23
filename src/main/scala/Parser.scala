object Parser:
  enum ParsingError:
    case InvalidInteger(value: String)             extends ParsingError
    case InvalidLetExpression(tokens: List[Token]) extends ParsingError
    case UnmatchedCase(tokens: List[Token])        extends ParsingError

  enum Expression:
    case Identifier          extends Expression
    case Integer(value: Int) extends Expression

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement

  type Node = Expression | Statement

  private val eatUntilExprEnd = eatUntil(List(Token.EOF, Token.Semicolon))

  def parse(tokens: List[Token]): Either[List[ParsingError], List[Node]] =
    @annotation.tailrec
    def doParse(
        tokens: List[Token],
        ast: List[Node],
        errors: List[ParsingError]
    ): Either[List[ParsingError], List[Node]] =
      tokens match
        case Nil | (Token.EOF :: Nil) =>
          if errors.isEmpty then Right(ast.reverse) else Left(errors.reverse)
        case all @ Token.Let
            :: Token.Identifier(identifier)
            :: Token.Assign
            :: rest =>
          parseExpression(rest) match
            case Left(reason) =>
              val (failedTokens, leftoverTokens) = eatUntilExprEnd(all)
              doParse(leftoverTokens, ast, reason :: errors)
            case Right((expr, leftoverTokens)) =>
              doParse(
                leftoverTokens,
                Statement.Let(identifier, expr) :: ast,
                errors
              )
        case other =>
          val (failedTokens, leftoverTokens) = eatUntilExprEnd(other)
          doParse(
            leftoverTokens,
            ast,
            ParsingError.UnmatchedCase(failedTokens) :: errors
          )

    doParse(tokens, List.empty, List.empty)

  private def parseExpression(tokens: List[Token])
      : Either[ParsingError, (Expression, List[Token])] =
    tokens match
      case Token.Integer(value) :: Token.Semicolon :: rest =>
        value.toIntOption match
          case None      => Left(ParsingError.InvalidInteger(value))
          case Some(int) => Right(Expression.Integer(int) -> rest)
      case other => Left(ParsingError.InvalidLetExpression(other))

  private def eatUntil(stopTokens: List[Token])(tokens: List[Token])
      : (List[Token], List[Token]) =
    @annotation.tailrec
    def doEat(
        tokens: List[Token],
        eaten: List[Token]
    ): (List[Token], List[Token]) =
      tokens match
        case Nil => (eaten.reverse, Nil)
        case token :: rest if stopTokens.contains(token) =>
          (eaten.reverse, rest)
        case token :: rest => doEat(rest, token :: eaten)

    doEat(tokens, List.empty)
