object Parser:
  import AST.*

  enum Precedence:
    case Lowest, Equals, LessGreater, Sum, Product, Prefix, Call

    def >=(other: Precedence): Boolean = this.ordinal >= other.ordinal

  object Precedence:
    def of(token: Token): Precedence =
      token match
        case (Token.Equal | Token.NotEqual)         => Equals
        case (Token.LesserThan | Token.GreaterThan) => LessGreater
        case (Token.Plus | Token.Minus)             => Sum
        case (Token.Asterisk | Token.Slash)         => Product
        case _                                      => Lowest

  enum ParsingError:
    case InvalidInteger(value: String)             extends ParsingError
    case InvalidLetExpression(tokens: List[Token]) extends ParsingError
    case InvalidExpression(tokens: List[Token])    extends ParsingError
    case NoPrefixExpression(token: Token)          extends ParsingError
    case NoInfixExpression(token: Token)           extends ParsingError
    case UnmatchedCase(tokens: List[Token])        extends ParsingError

  def parse(tokens: List[Token]): Either[List[ParsingError], Program] =
    parse(tokens, List.empty, List.empty).map(Program(_))

  @annotation.tailrec
  private def parse(
      tokens: List[Token],
      ast: List[Node],
      errors: List[ParsingError]
  ): Either[List[ParsingError], List[Node]] =
    tokens match
      case Nil | (Token.EOF :: Nil) =>
        if errors.isEmpty
        then Right(ast.reverse)
        else Left(errors.reverse)
      // Keep it for now, until the rest of the parsing is not fully fleshed out
      case Token.Semicolon :: rest => parse(rest, ast, errors)
      case all @ Token.Let :: rest =>
        parseLetStatement(rest) match
          case Left(error) =>
            parse(eatUntilExprEnd(all), ast, error :: errors)
          case Right((statement, leftoverTokens)) =>
            parse(leftoverTokens, statement :: ast, errors)
      case all @ Token.Return :: rest =>
        parseExpression(rest, Precedence.Lowest) match
          case Left(error) =>
            parse(eatUntilExprEnd(all), ast, error :: errors)
          case Right((expr, leftoverTokens)) =>
            parse(leftoverTokens, Statement.Return(expr) :: ast, errors)
      case other =>
        parseExpression(other, Precedence.Lowest) match
          case Left(error) =>
            parse(eatUntilExprEnd(other), ast, error :: errors)
          case Right((expression, leftoverTokens)) =>
            parse(leftoverTokens, Statement.Expr(expression) :: ast, errors)

  private def parseLetStatement(tokens: List[Token]) =
    tokens match
      case all @ Token.Identifier(identifier)
          :: Token.Assign
          :: rest => parseExpression(rest, Precedence.Lowest).map:
          (expression, leftoverTokens) =>
            Statement.Let(identifier, expression) -> leftoverTokens
      case _ => Left(ParsingError.InvalidLetExpression(tokens))

  private def parseExpression(
      tokens: List[Token],
      precedence: Precedence
  ): Either[ParsingError, (Expression, List[Token])] =
    @annotation.tailrec
    def doParseInfixExpr(
        expr: Expression,
        tokens: List[Token]
    ): Either[ParsingError, (Expression, List[Token])] =
      tokens match
        case Nil | (Token.EOF :: Nil) => Right(expr -> Nil)
        case Token.Semicolon :: rest  => Right(expr -> tokens)
        case all @ (token :: rest) if precedence >= Precedence.of(token) =>
          Right(expr -> all)
        case other =>
          parseInfixExpression(expr, other) match
            case error @ Left(_) => error
            case Right((expression, leftoverTokens)) =>
              doParseInfixExpr(expression, leftoverTokens)

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
          case Some(int) => Right(Expression.IntegerLiteral(int) -> rest)
      case Token.True :: rest =>
        Right(Expression.BooleanLiteral(true) -> rest)
      case Token.False :: rest =>
        Right(Expression.BooleanLiteral(false) -> rest)
      case (token @ (Token.Bang | Token.Minus)) :: rest =>
        parseExpression(rest, Precedence.Prefix) match
          case error @ Left(_) => error
          case Right((expression, leftoverTokens)) =>
            Right(Expression.PrefixOperator(token, expression), leftoverTokens)
      case all @ Token.LeftParen :: rest =>
        parseExpression(rest, Precedence.Lowest).flatMap:
          case (expression, Token.RightParen :: rest) =>
            Right(expression -> rest)
          case _ => Left(ParsingError.InvalidExpression(all))
      case token :: rest => Left(ParsingError.NoPrefixExpression(token))

  private def parseInfixExpression(
      left: Expression,
      tokens: List[Token]
  ): Either[ParsingError, (Expression, List[Token])] =
    tokens match
      case Nil => Left(ParsingError.InvalidExpression(Nil))
      case (token @ (Token.Plus
          | Token.Minus
          | Token.Asterisk
          | Token.Slash
          | Token.Equal
          | Token.NotEqual
          | Token.LesserThan
          | Token.GreaterThan)) :: rest =>
        parseExpression(rest, Precedence.of(token)).map:
          (right, leftoverTokens) =>
            Expression.InfixOperator(left, token, right) -> leftoverTokens
      case token :: _ => Left(ParsingError.NoInfixExpression(token))

  private val eatUntilExprEnd = eatUntil(List(Token.EOF, Token.Semicolon))

  private def eatUntil(stopTokens: List[Token])(tokens: List[Token])
      : List[Token] =
    @annotation.tailrec
    def doEat(tokens: List[Token]): List[Token] =
      tokens match
        case Nil                                         => Nil
        case token :: rest if stopTokens.contains(token) => rest
        case _ :: rest                                   => doEat(rest)

    doEat(tokens)
