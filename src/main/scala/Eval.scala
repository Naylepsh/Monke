import AST.*

object Eval:
  enum EvalutationError:
    case InvalidSyntax(node: Node)         extends EvalutationError
    case UndefinedIdentifier(name: String) extends EvalutationError

  type Environment = List[(String, MonkeyObject)]
  object Environment:
    def empty: Environment = List.empty

    extension (env: Environment)
      def find(name: String)
          : Either[EvalutationError.UndefinedIdentifier, MonkeyObject] =
        (env.find: identifier =>
          identifier._1 == name) match
          case None             => Left(EvalutationError.UndefinedIdentifier(name))
          case Some((_, value)) => Right(value)

  def eval(
      program: AST.Program,
      env: Environment = Environment.empty
  ): Either[EvalutationError, (MonkeyObject, Environment)] =
    evalNodes(program.nodes, env, None).map:
      case (None, _)                   => (MonkeyObject.Null, env)
      case (Some(result), environment) => (result, environment)

  @annotation.tailrec
  private def evalNodes(
      nodes: List[Node],
      environment: Environment,
      result: Option[MonkeyObject]
  ): Either[EvalutationError, (Option[MonkeyObject], Environment)] =
    nodes match
      case Nil => Right(result, environment)
      case node :: rest =>
        eval(node, environment) match
          case Left(error) => Left(error)
          case Right(MonkeyObject.ReturnValue(value), env) =>
            Right(Some(value), env)
          case Right(result, env) => evalNodes(rest, env, Some(result))

  import Environment.find

  private def eval(
      node: Node,
      env: Environment
  ): Either[EvalutationError, (MonkeyObject, Environment)] =
    node match
      case Expression.IntegerLiteral(value) =>
        Right(MonkeyObject.IntegerLiteral(value), env)
      case Expression.BooleanLiteral(value) =>
        Right(MonkeyObject.of(value), env)
      case Expression.Identifier(identifier) =>
        env.find(identifier).map: value =>
          (value, env)
      case Statement.Expr(expr) => eval(expr, env)
      case Expression.PrefixOperator(token, expr) =>
        evalPrefixExpression(token, expr, node, env).map: result =>
          (result, env)
      case Expression.InfixOperator(left, token, right) =>
        evalInfixExpression(left, token, right, node, env).map: result =>
          (result, env)
      case Expression.If(condition, consequence, alternative) =>
        evalIfExpression(condition, consequence, alternative, env).map:
          result =>
            (result, env)
      case Statement.Block(nodes) =>
        evalBlockStatement(nodes, env).map: result =>
          (result, env)
      case Statement.Return(expr) =>
        eval(expr, env).map: (expr, env) =>
          (MonkeyObject.ReturnValue(expr), env)
      case Statement.Let(identifier, expr) =>
        eval(expr, env).map: (result, _) =>
          (MonkeyObject.Null, (identifier -> result) :: env)

  private def evalBlockStatement(nodes: List[Node], env: Environment) =
    @annotation.tailrec
    def evalBlock(
        nodes: List[Node],
        env: Environment,
        result: Option[MonkeyObject]
    ): Either[EvalutationError, Option[MonkeyObject]] =
      nodes match
        case Nil => Right(result)
        case node :: rest =>
          eval(node, env) match
            case Left(error) => Left(error)
            case Right(returnValue @ MonkeyObject.ReturnValue(value), _) =>
              Right(Some(returnValue))
            case Right(result, env) => evalBlock(rest, env, Some(result))

    evalBlock(nodes, env, None).map(_.getOrElse(MonkeyObject.Null))

  private def evalIfExpression(
      condition: Expression,
      consequence: Statement.Block,
      alternative: Option[Statement.Block],
      env: Environment
  ) =
    eval(condition, env).flatMap: (condValue, _) =>
      if MonkeyObject.isTruthy(condValue)
      then
        eval(consequence, env).map: (result, _) =>
          result
      else
        alternative
          .map: alt =>
            eval(alt, env).map: (result, _) =>
              result
          .getOrElse(Right(MonkeyObject.Null))

  private def evalPrefixExpression(
      token: Token,
      expr: Expression,
      node: Node,
      env: Environment
  ) =
    token match
      case Token.Bang =>
        eval(expr, env).map: (value, _) =>
          MonkeyObject.of(!MonkeyObject.isTruthy(value))
      case Token.Minus =>
        eval(expr, env).flatMap:
          case (MonkeyObject.IntegerLiteral(value), _) =>
            Right(MonkeyObject.IntegerLiteral(-value))
          case _ => Left(EvalutationError.InvalidSyntax(node))
      case _ => Left(EvalutationError.InvalidSyntax(node))

  private def evalInfixExpression(
      left: Expression,
      token: Token,
      right: Expression,
      node: Node,
      env: Environment
  ) =
    (eval(left, env), eval(right, env)) match
      case (
            Right(MonkeyObject.IntegerLiteral(leftVal), _),
            Right(MonkeyObject.IntegerLiteral(rightVal), _)
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
            Right(MonkeyObject.BooleanLiteral(leftVal), _),
            Right(MonkeyObject.BooleanLiteral(rightVal), _)
          ) =>
        token match
          case Token.Equal    => Right(MonkeyObject.of(leftVal == rightVal))
          case Token.NotEqual => Right(MonkeyObject.of(leftVal != rightVal))
          case _              => Left(EvalutationError.InvalidSyntax(node))
      case _ => Left(EvalutationError.InvalidSyntax(node))
