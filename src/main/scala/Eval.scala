import AST.*

object Eval:
  enum EvalutationError:
    case InvalidSyntax(node: Node) extends EvalutationError

  def eval(program: AST.Program): Either[EvalutationError, MonkeyObject] =
    evalNodes(program.nodes, None).map(_.getOrElse(MonkeyObject.Null))

  @annotation.tailrec
  private def evalNodes(
      nodes: List[Node],
      result: Option[MonkeyObject]
  ): Either[EvalutationError, Option[MonkeyObject]] =
    nodes match
      case Nil => Right(result)
      case node :: rest =>
        eval(node) match
          case Left(error)   => Left(error)
          case Right(result) => evalNodes(rest, Some(result))

  private def eval(node: Node): Either[EvalutationError, MonkeyObject] =
    node match
      case Expression.IntegerLiteral(value) =>
        Right(MonkeyObject.IntegerLiteral(value))
      case Expression.BooleanLiteral(value) =>
        Right(MonkeyObject.of(value))
      case Statement.Expr(expr) => eval(expr)
      case Expression.PrefixOperator(token, expr) =>
        evalPrefixExpression(token, expr, node)
      case Expression.InfixOperator(left, token, right) =>
        evalInfixExpression(left, token, right, node)
      case Expression.If(condition, consequence, alternative) =>
        evalIfExpression(condition, consequence, alternative)
      case Statement.Block(nodes) =>
        evalNodes(nodes, None).map(_.getOrElse(MonkeyObject.Null))

  private def evalIfExpression(
      condition: Expression,
      consequence: Statement.Block,
      alternative: Option[Statement.Block]
  ) =
    eval(condition).flatMap: condValue =>
      if MonkeyObject.isTruthy(condValue)
      then eval(consequence)
      else alternative.map(eval).getOrElse(Right(MonkeyObject.Null))

  private def evalPrefixExpression(
      token: Token,
      expr: Expression,
      node: Node
  ) =
    token match
      case Token.Bang =>
        eval(expr).map: value =>
          MonkeyObject.of(!MonkeyObject.isTruthy(value))
      case Token.Minus =>
        eval(expr).flatMap:
          case MonkeyObject.IntegerLiteral(value) =>
            Right(MonkeyObject.IntegerLiteral(-value))
          case _ => Left(EvalutationError.InvalidSyntax(node))
      case _ => Left(EvalutationError.InvalidSyntax(node))

  private def evalInfixExpression(
      left: Expression,
      token: Token,
      right: Expression,
      node: Node
  ) =
    (eval(left), eval(right)) match
      case (
            Right(MonkeyObject.IntegerLiteral(leftVal)),
            Right(MonkeyObject.IntegerLiteral(rightVal))
          ) =>
        token match
          case Token.Plus =>
            Right(MonkeyObject.IntegerLiteral(leftVal + rightVal))
          case Token.Minus =>
            Right(MonkeyObject.IntegerLiteral(leftVal - rightVal))
          case Token.Asterisk =>
            Right(MonkeyObject.IntegerLiteral(leftVal * rightVal))
          case Token.Slash =>
            Right(MonkeyObject.IntegerLiteral(leftVal / rightVal))
          case Token.Equal =>
            Right(MonkeyObject.of(leftVal == rightVal))
          case Token.NotEqual =>
            Right(MonkeyObject.of(leftVal != rightVal))
          case Token.LesserThan =>
            Right(MonkeyObject.of(leftVal < rightVal))
          case Token.GreaterThan =>
            Right(MonkeyObject.of(leftVal > rightVal))
          case _ => Left(EvalutationError.InvalidSyntax(node))
      case (
            Right(MonkeyObject.BooleanLiteral(leftVal)),
            Right(MonkeyObject.BooleanLiteral(rightVal))
          ) =>
        token match
          case Token.Equal    => Right(MonkeyObject.of(leftVal == rightVal))
          case Token.NotEqual => Right(MonkeyObject.of(leftVal != rightVal))
          case _              => Left(EvalutationError.InvalidSyntax(node))
      case _ => Left(EvalutationError.InvalidSyntax(node))
