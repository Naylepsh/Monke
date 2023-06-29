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
    case InvalidInteger(value: String)              extends ParsingError
    case InvalidLetExpression(tokens: List[Token])  extends ParsingError
    case InvalidExpression(tokens: List[Token])     extends ParsingError
    case NoPrefixExpression(token: Token)           extends ParsingError
    case NoInfixExpression(token: Token)            extends ParsingError
    case UnmatchedCase(tokens: List[Token])         extends ParsingError
    case MissingClosingBracket(tokens: List[Token]) extends ParsingError
    case InvalidIfExpression(tokens: List[Token])   extends ParsingError
    /* Internal errors */
    case InvalidBlock(tokens: List[Token]) extends ParsingError

  def parse(tokens: List[Token]): Either[List[ParsingError], Program] =
    parse(tokens, List.empty, List.empty).map(Program(_))

  @annotation.tailrec
  private def parse(
      tokens: List[Token],
      ast: List[Node],
      errors: List[ParsingError]
  ): Either[List[ParsingError], List[Node]] =
    tokens match
      case Nil =>
        if errors.isEmpty
        then Right(ast.reverse)
        else Left(errors.reverse)
      case tokens =>
        nextNode(tokens) match
          case Right(None, leftoverTokens) =>
            parse(leftoverTokens, ast, errors)
          case Right(Some(node), leftoverTokens) =>
            parse(leftoverTokens, node :: ast, errors)
          case Left(newErrors) =>
            parse(eatUntilExprEnd(tokens), ast, newErrors ::: errors)

  private def nextNode(tokens: List[Token])
      : Either[List[ParsingError], (Option[Node], List[Token])] =
    tokens match
      case Nil | (Token.EOF :: Nil) => Right(None -> List.empty)
      // Keep it for now, until the rest of the parsing is not fully fleshed out
      case Token.Semicolon :: rest => Right(None -> rest)
      case all @ Token.Let :: rest =>
        parseLetStatement(rest).map: (expr, leftoverTokens) =>
          Some(expr) -> leftoverTokens
      case all @ Token.Return :: rest =>
        parseExpression(rest, Precedence.Lowest).map: (expr, leftoverTokens) =>
          Some(Statement.Return(expr)) -> leftoverTokens
      case other =>
        parseExpression(other, Precedence.Lowest).map: (expr, leftoverTokens) =>
          Some(Statement.Expr(expr)) -> leftoverTokens

  private def parseLetStatement(tokens: List[Token]) =
    tokens match
      case all @ Token.Identifier(identifier)
          :: Token.Assign
          :: rest => parseExpression(rest, Precedence.Lowest).map:
          (expression, leftoverTokens) =>
            Statement.Let(identifier, expression) -> leftoverTokens
      case _ => Left(List(ParsingError.InvalidLetExpression(tokens)))

  private def parseExpression(
      tokens: List[Token],
      precedence: Precedence
  ): Either[List[ParsingError], (Expression, List[Token])] =
    @annotation.tailrec
    def doParseInfixExpr(
        expr: Expression,
        tokens: List[Token]
    ): Either[List[ParsingError], (Expression, List[Token])] =
      tokens match
        case Nil | (Token.EOF :: Nil) => Right(expr -> Nil)
        case Token.Semicolon :: rest  => Right(expr -> tokens)
        case all @ (token :: rest) if precedence >= Precedence.of(token) =>
          Right(expr -> all)
        case other =>
          parseInfixExpression(expr, other) match
            case errors @ Left(_) => errors
            case Right((expression, leftoverTokens)) =>
              doParseInfixExpr(expression, leftoverTokens)

    parsePrefixExpression(tokens, precedence).flatMap(doParseInfixExpr(_, _))

  private def parsePrefixExpression(
      tokens: List[Token],
      precedence: Precedence
  ): Either[List[ParsingError], (Expression, List[Token])] =
    tokens match
      case Nil => Left(List(ParsingError.InvalidExpression(Nil)))
      case Token.Identifier(value) :: rest =>
        Right(Expression.Identifier(value) -> rest)
      case Token.Integer(value) :: rest =>
        value.toIntOption match
          case None      => Left(List(ParsingError.InvalidInteger(value)))
          case Some(int) => Right(Expression.IntegerLiteral(int) -> rest)
      case Token.True :: rest =>
        Right(Expression.BooleanLiteral(true) -> rest)
      case Token.False :: rest =>
        Right(Expression.BooleanLiteral(false) -> rest)
      case (token @ (Token.Bang | Token.Minus)) :: rest =>
        parseExpression(rest, Precedence.Prefix).map:
          (expression, leftoverTokens) =>
            Expression.PrefixOperator(token, expression) -> leftoverTokens
      case all @ Token.LeftParen :: rest =>
        parseExpression(rest, Precedence.Lowest)
          .flatMap:
            case (expression, Token.RightParen :: rest) =>
              Right(expression -> rest)
            case _ => Left(List(ParsingError.InvalidExpression(all)))
      case all @ Token.If :: rest =>
        parseIfExpression(rest)
      case token :: rest => Left(List(ParsingError.NoPrefixExpression(token)))

  private def parseIfExpression(tokens: List[Token]) =
    tokens match
      case Token.LeftParen :: rest =>
        parseExpression(rest, Precedence.Lowest).flatMap:
          case (
                condition,
                Token.RightParen :: leftoverTokens
              ) =>
            parseIfBodies(leftoverTokens).map:
              case (consequence, alternative, leftoverTokens) =>
                Expression.If(
                  condition,
                  consequence,
                  alternative
                ) -> leftoverTokens
          case (_, other) =>
            Left(List(ParsingError.InvalidIfExpression(eatUntilExprEnd(other))))
      case other => Left(List(ParsingError.InvalidIfExpression(tokens)))

  private def parseIfBodies(tokens: List[Token]) =
    parseBlockStatement(tokens).flatMap:
      case (consequence, Token.Else :: leftoverTokens) =>
        parseBlockStatement(leftoverTokens).map:
          (alternative, leftoverTokens) =>
            (consequence, Some(alternative), leftoverTokens)
      case (consequence, leftoverTokens) =>
        Right(consequence, None, leftoverTokens)

  private def parseBlockStatement(tokens: List[Token]) =
    @annotation.tailrec
    def parseBlock(
        currentTokens: List[Token],
        ast: List[Node],
        errors: List[ParsingError]
    ): Either[List[ParsingError], (List[Node], List[Token])] =
      currentTokens match
        case Nil =>
          Left((ParsingError.MissingClosingBracket(tokens) :: errors).reverse)
        case Token.RightBrace :: rest =>
          if errors.isEmpty
          then Right(ast.reverse -> rest)
          else Left(errors.reverse)
        case currentTokens =>
          nextNode(currentTokens) match
            case Right(None, leftoverTokens) =>
              parseBlock(leftoverTokens, ast, errors)
            case Right(Some(node), leftoverTokens) =>
              parseBlock(leftoverTokens, node :: ast, errors)
            case Left(newErrors) =>
              parseBlock(
                eatUntilExprEnd(currentTokens),
                ast,
                newErrors ::: errors
              )

    tokens match
      case _ :: tokens =>
        parseBlock(tokens, List.empty, List.empty)
          .map: (nodes, leftoverTokens) =>
            Statement.Block(nodes) -> leftoverTokens
      case _ => Left(List(ParsingError.InvalidBlock(tokens)))

  private def parseInfixExpression(
      left: Expression,
      tokens: List[Token]
  ): Either[List[ParsingError], (Expression, List[Token])] =
    tokens match
      case Nil => Left(List(ParsingError.InvalidExpression(Nil)))
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
      case token :: _ => Left(List(ParsingError.NoInfixExpression(token)))

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
