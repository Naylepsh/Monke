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
        case Token.LeftParen                        => Call
        case _                                      => Lowest

  enum ParsingError:
    case InvalidInteger(value: String)                  extends ParsingError
    case InvalidLetExpression(tokens: List[Token])      extends ParsingError
    case InvalidExpression(tokens: List[Token])         extends ParsingError
    case NoPrefixExpression(token: Token)               extends ParsingError
    case NoInfixExpression(token: Token)                extends ParsingError
    case UnmatchedCase(tokens: List[Token])             extends ParsingError
    case MissingClosingBracket(tokens: List[Token])     extends ParsingError
    case InvalidIfExpression(tokens: List[Token])       extends ParsingError
    case InvalidFunctionExpression(tokens: List[Token]) extends ParsingError
    case InvalidFunctionParameters(tokens: List[Token]) extends ParsingError
    case InvalidCallArguments(tokens: List[Token])      extends ParsingError
    case InvalidBlock(tokens: List[Token])              extends ParsingError

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
      case all @ Token.Let :: _ =>
        parseLetStatement(all).map: (expr, leftoverTokens) =>
          Some(expr) -> leftoverTokens
      case all @ Token.Return :: rest =>
        parseExpression(rest, Precedence.Lowest).map: (expr, leftoverTokens) =>
          Some(Statement.Return(expr)) -> leftoverTokens
      case other =>
        parseExpression(other, Precedence.Lowest).map: (expr, leftoverTokens) =>
          Some(Statement.Expr(expr)) -> leftoverTokens

  private def parseLetStatement(tokens: List[Token]) =
    tokens match
      case _ :: (all @ Token.Identifier(identifier) :: Token.Assign :: rest) =>
        parseExpression(rest, Precedence.Lowest).map:
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
      case Token.Str(value) :: rest =>
        Right(Expression.StringLiteral(value) -> rest)
      case Token.True :: rest =>
        Right(Expression.BooleanLiteral(true) -> rest)
      case Token.False :: rest =>
        Right(Expression.BooleanLiteral(false) -> rest)
      case (token @ (Token.Bang | Token.Minus)) :: rest =>
        parseExpression(rest, Precedence.Prefix).map:
          (expression, leftoverTokens) =>
            Expression.PrefixOperator(token, expression) -> leftoverTokens
      case all @ Token.LeftParen :: rest =>
        parseExpression(rest, Precedence.Lowest).flatMap:
          case (expression, Token.RightParen :: rest) =>
            Right(expression -> rest)
          case _ => Left(List(ParsingError.InvalidExpression(all)))
      case all @ Token.If :: _   => parseIfExpression(all)
      case all @ Token.Func :: _ => parseFunctionExpression(all)
      case token :: rest         => Left(List(ParsingError.NoPrefixExpression(token)))

  private def parseIfExpression(tokens: List[Token]) =
    tokens match
      case _ :: Token.LeftParen :: rest =>
        parseExpression(rest, Precedence.Lowest).flatMap:
          case (condition, Token.RightParen :: leftoverTokens) =>
            parseIfBodies(leftoverTokens).map:
              case (consequence, alternative, leftoverTokens) =>
                val ifExpr = Expression.If(condition, consequence, alternative)
                ifExpr -> leftoverTokens
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

  private def parseFunctionExpression(tokens: List[Token]) =
    tokens match
      case _ :: (rest @ Token.LeftParen :: _) =>
        parseFunctionParameters(rest).flatMap: (parameters, leftoverTokens) =>
          leftoverTokens match
            case all @ Token.LeftBrace :: rest =>
              parseBlockStatement(all).map: (body, leftoverTokens) =>
                Expression.Func(parameters, body) -> leftoverTokens
            case _ => Left(List(ParsingError.InvalidBlock(leftoverTokens)))
      case _ => Left(List(ParsingError.InvalidFunctionExpression(tokens)))

  private def parseFunctionParameters(tokens: List[Token]) =
    @annotation.tailrec
    def parseParameters(
        tokens: List[Token],
        parameters: List[Expression.Identifier]
    ): Either[List[ParsingError], (List[Expression.Identifier], List[Token])] =
      tokens match
        case Token.RightParen :: rest =>
          Right(parameters.reverse -> rest)
        case Token.Identifier(value) :: Token.RightParen :: rest =>
          val identifier: Expression.Identifier = Expression.Identifier(value)
          Right((identifier :: parameters).reverse -> rest)
        case Token.Identifier(value) :: Token.Comma :: rest =>
          parseParameters(rest, Expression.Identifier(value) :: parameters)
        case invalidTokens =>
          Left(List(ParsingError.InvalidFunctionParameters(invalidTokens)))

    tokens match
      case _ :: tokens => parseParameters(tokens, List.empty)
      case _           => Left(List(ParsingError.InvalidFunctionParameters(tokens)))

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
      case all @ Token.LeftParen :: _ =>
        parseCallArguments(all).map: (args, leftoverTokens) =>
          Expression.Call(left, args) -> leftoverTokens
      case token :: _ => Left(List(ParsingError.NoInfixExpression(token)))

  private def parseCallArguments(tokens: List[Token]) =
    @annotation.tailrec
    def parseArguments(
        tokens: List[Token],
        args: List[Expression]
    ): Either[List[ParsingError], (List[Expression], List[Token])] =
      tokens match
        case Nil                      => Left(List(ParsingError.InvalidCallArguments(tokens)))
        case Token.RightParen :: rest => Right(args.reverse -> rest)
        case tokens => parseExpression(tokens, Precedence.Lowest) match
            case Left(errors) => Left(errors)
            case Right(arg, Token.RightParen :: rest) =>
              Right((arg :: args).reverse -> rest)
            case Right(arg, Token.Comma :: rest) =>
              parseArguments(rest, arg :: args)
            case Right(_, invalidTokens) =>
              Left(List(ParsingError.InvalidCallArguments(invalidTokens)))

    tokens match
      case _ :: tokens => parseArguments(tokens, List.empty)
      case _           => Left(List(ParsingError.InvalidCallArguments(tokens)))

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
