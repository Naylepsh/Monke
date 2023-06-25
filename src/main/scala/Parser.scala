object Parser:
  enum ParsingError:
    case InvalidInteger(value: String)             extends ParsingError
    case InvalidLetExpression(tokens: List[Token]) extends ParsingError
    case InvalidExpression(tokens: List[Token])    extends ParsingError
    case NoPrefixExpression(token: Token)          extends ParsingError
    case UnmatchedCase(tokens: List[Token])        extends ParsingError

  enum Expression:
    case Identifier(value: String) extends Expression
    case Integer(value: Int)       extends Expression
    case PrefixOperator(token: Token, expression: Expression)
        extends Expression
    case InfixOperator(left: Expression, token: Token, right: Expression)
        extends Expression

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement
    case Return(expression: Expression)                  extends Statement
    case Expr(expression: Expression)                    extends Statement

  type Node = Expression | Statement

  enum Precedence:
    case Lowest, Equals, LessGreater, Sum, Product, Prefix, Call

    def >=(other: Precedence): Boolean = this.ordinal >= other.ordinal

  object Precedence:
    def of(token: Token): Precedence =
      token match
        case (Token.Equal | Token.NotEqual)         => Equals
        case (Token.LesserThan | Token.GreaterThan) => LessGreater
        case (Token.Plus | Token.Minus)             => Sum
        case (Token.Asterisk | Token.Slash)           => Product
        case _                                      => Lowest

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
        // Keep it for now, until the rest of the parsing is not fully fleshed out
        case Token.Semicolon :: rest => doParse(rest, ast, errors)
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
        case all @ Token.Return :: rest =>
          parseExpression(rest) match
            case Left(reason) =>
              val (failedTokens, leftoverTokens) = eatUntilExprEnd(all)
              doParse(leftoverTokens, ast, reason :: errors)
            case Right((expr, leftoverTokens)) =>
              doParse(leftoverTokens, Statement.Return(expr) :: ast, errors)
        case other =>
          parseExpressionV2(other, Precedence.Lowest) match
            case Left(reason) =>
              val (failedTokens, leftoverTokens) = eatUntilExprEnd(other)
              doParse(leftoverTokens, ast, reason :: errors)
            case Right((expression, leftoverTokens)) =>
              doParse(leftoverTokens, Statement.Expr(expression) :: ast, errors)

    doParse(tokens, List.empty, List.empty)

  private def parseExpressionV2(
      tokens: List[Token],
      precedence: Precedence
  ): Either[ParsingError, (Expression, List[Token])] =
    def doParseInfixExpr(
        expr: Expression,
        tokens: List[Token]
    ): Either[ParsingError, (Expression, List[Token])] =
      tokens match
        case Nil | (Token.EOF :: Nil) => Right(expr -> Nil)
        case Token.Semicolon :: rest  => Right(expr -> rest)
        case all @ (token :: rest) if precedence >= Precedence.of(token) =>
          Right(expr -> all)
        case other =>
          parseInfixExpression(expr, other).flatMap(doParseInfixExpr(_, _))

    parsePrefixExpression(tokens, precedence).flatMap(doParseInfixExpr(_, _))

  private def parsePrefixExpression(
      tokens: List[Token],
      precedence: Precedence
  ): Either[ParsingError, (Expression, List[Token])] =
    tokens match
      case Nil => Left(ParsingError.InvalidExpression(Nil))
      case Token.Identifier(value) :: rest =>
        Right(Expression.Identifier(value) -> rest)
      case Token.Integer(value) :: rest =>
        value.toIntOption match
          case None      => Left(ParsingError.InvalidInteger(value))
          case Some(int) => Right(Expression.Integer(int) -> rest)
      case (token @ (Token.Bang | Token.Minus)) :: rest =>
        parseExpressionV2(rest, Precedence.Prefix) match
          case reason @ Left(_) => reason
          case Right((expression, leftoverTokens)) =>
            Right(Expression.PrefixOperator(token, expression), leftoverTokens)
      case token :: rest => Left(ParsingError.NoPrefixExpression(token))

  private def parseInfixExpression(
      left: Expression,
      tokens: List[Token]
  ): Either[ParsingError, (Expression, List[Token])] =
    tokens match
      case Nil => Left(ParsingError.InvalidExpression(Nil))
      case (token @ (Token.Plus | Token.Minus | Token.Asterisk | Token.Slash | Token.Equal | Token.NotEqual | Token.LesserThan | Token.GreaterThan)) :: rest =>
        parseExpressionV2(rest, Precedence.of(token)).map:
          (right, leftoverTokens) =>
            Expression.InfixOperator(left, token, right) -> leftoverTokens

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
