object Parser:
  enum Expression:
    case Identifier          extends Expression
    case Integer(value: Int) extends Expression

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement

  type Node = Expression | Statement

  def parse(tokens: List[Token]): List[Node] =
    @annotation.tailrec
    def doParse(tokens: List[Token], ast: List[Node]): List[Node] =
      tokens match
        case Nil | (Token.EOF :: Nil) => ast.reverse
        case Token.Let
            :: Token.Identifier(identifier)
            :: Token.Assign
            :: rest =>
          val (expr, leftoverTokens) = parseExpression(rest)
          doParse(leftoverTokens, Statement.Let(identifier, expr) :: ast)

    doParse(tokens, List.empty)

  private def parseExpression(tokens: List[Token]): (Expression, List[Token]) =
    tokens match
      case Token.Integer(value) :: Token.Semicolon :: rest =>
        // TODO: make it safe (use toIntOption, return custom parsing error on failure)
        Expression.Integer(value.toInt) -> rest
      case _ => assert(false, "oopsie")
