import AST.*

object Eval:
  enum EvalutationError:
    case InvalidSyntax(node: Node) extends EvalutationError

  def eval(program: AST.Program): Either[EvalutationError, MonkeyObject] =
    @annotation.tailrec
    def doEval(
        nodes: List[Node],
        result: Option[MonkeyObject]
    ): Either[EvalutationError, Option[MonkeyObject]] =
      nodes match
        case Nil => Right(result)
        case node :: rest =>
          eval(node) match
            case Left(error)   => Left(error)
            case Right(result) => doEval(rest, Some(result))

    doEval(program.nodes, None).map(_.getOrElse(MonkeyObject.Null))

  def eval(node: Node): Either[EvalutationError, MonkeyObject] =
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

  private def evalPrefixExpression(
      token: Token,
      expr: Expression,
      node: Node
  ) =
    token match
      case Token.Bang =>
        eval(expr).flatMap:
          case MonkeyObject.BooleanLiteral(value) =>
            Right(MonkeyObject.of(!value))
          case MonkeyObject.IntegerLiteral(0) => Right(MonkeyObject.of(true))
          case MonkeyObject.IntegerLiteral(_) => Right(MonkeyObject.of(false))
          case MonkeyObject.Null              => Right(MonkeyObject.of(true))
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
