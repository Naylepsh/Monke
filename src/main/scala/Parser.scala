object Parser:
  enum ParsingError:
    case InvalidInteger(value: String)             extends ParsingError
    case InvalidLetExpression(tokens: List[Token]) extends ParsingError

  enum Expression:
    case Identifier          extends Expression
    case Integer(value: Int) extends Expression

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement

  type Node = Expression | Statement

  def parse(tokens: List[Token]): Either[ParsingError, List[Node]] =
    @annotation.tailrec
    def doParse(
        tokens: List[Token],
        ast: List[Node]
    ): Either[ParsingError, List[Node]] =
      tokens match
        case Nil | (Token.EOF :: Nil) => Right(ast.reverse)
        case Token.Let
            :: Token.Identifier(identifier)
            :: Token.Assign
            :: rest =>
          parseExpression(rest) match
            case Left(reason) => Left(reason)
            case Right((expr, leftoverTokens)) =>
              doParse(leftoverTokens, Statement.Let(identifier, expr) :: ast)

    doParse(tokens, List.empty)

  private def parseExpression(tokens: List[Token])
      : Either[ParsingError, (Expression, List[Token])] =
    tokens match
      case Token.Integer(value) :: Token.Semicolon :: rest =>
        value.toIntOption match
          case None      => Left(ParsingError.InvalidInteger(value))
          case Some(int) => Right(Expression.Integer(int) -> rest)
      case other => Left(ParsingError.InvalidLetExpression(other))
