object Parser:
  enum ParsingError:
    case InvalidInteger(value: String)             extends ParsingError
    case InvalidLetExpression(tokens: List[Token]) extends ParsingError
    case InvalidExpression(tokens: List[Token])    extends ParsingError
    case NoPrefixExpression(token: Token)          extends ParsingError
    case NoInfixExpression(token: Token)           extends ParsingError
    case UnmatchedCase(tokens: List[Token])        extends ParsingError

  enum Expression:
    case Identifier(value: String) extends Expression
    case Integer(value: Int)       extends Expression
    case PrefixOperator(token: Token, expression: Expression)
        extends Expression
    case InfixOperator(left: Expression, token: Token, right: Expression)
        extends Expression

  object Expression:
    given showExpression(using showToken: Show[Token]): Show[Expression] with
      def show(expr: Expression): String =
        expr match
          case Identifier(value) => value
          case Integer(value)    => value.toString
          case PrefixOperator(token, expression) =>
            s"(${showToken.show(token)}${show(expression)})"
          case InfixOperator(left, token, right) =>
            s"(${show(left)} ${showToken.show(token)} ${show(right)})"

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement
    case Return(expression: Expression)                  extends Statement
    case Expr(expression: Expression)                    extends Statement

  object Statement:
    given showStatement(
        using showExpression: Show[Expression],
        showToken: Show[Token]
    ): Show[Statement] with
      def show(statement: Statement): String =
        statement match
          case Let(identifier, expression) =>
            s"let $identifier = ${showExpression.show(expression)}"
          case Return(expression) =>
            s"return ${showExpression.show(expression)}"
          case Expr(expression) => showExpression.show(expression)

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
        case (Token.Asterisk | Token.Slash)         => Product
        case _                                      => Lowest

  def parse(tokens: List[Token]): Either[List[ParsingError], List[Node]] =
    parse(tokens, List.empty, List.empty)

  @annotation.tailrec
  private def parse(
      tokens: List[Token],
      ast: List[Node],
      errors: List[ParsingError]
  ): Either[List[ParsingError], List[Node]] =
    tokens match
      case Nil | (Token.EOF :: Nil) =>
        if errors.isEmpty then Right(ast.reverse) else Left(errors.reverse)
      // Keep it for now, until the rest of the parsing is not fully fleshed out
      case Token.Semicolon :: rest => parse(rest, ast, errors)
      case all @ Token.Let :: rest =>
        parseLetStatement(rest) match
          case Left(reason) =>
            parse(eatUntilExprEnd(all), ast, reason :: errors)
          case Right((statement, leftoverTokens)) =>
            parse(leftoverTokens, statement :: ast, errors)
      case all @ Token.Return :: rest =>
        parseExpression(rest, Precedence.Lowest) match
          case Left(reason) =>
            parse(eatUntilExprEnd(all), ast, reason :: errors)
          case Right((expr, leftoverTokens)) =>
            parse(leftoverTokens, Statement.Return(expr) :: ast, errors)
      case other =>
        parseExpression(other, Precedence.Lowest) match
          case Left(reason) =>
            parse(eatUntilExprEnd(other), ast, reason :: errors)
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
            case failure @ Left(_) => failure
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
          case Some(int) => Right(Expression.Integer(int) -> rest)
      case (token @ (Token.Bang | Token.Minus)) :: rest =>
        parseExpression(rest, Precedence.Prefix) match
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
