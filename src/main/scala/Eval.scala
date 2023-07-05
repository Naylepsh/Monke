import AST.*

object Eval:
  enum EvalutationError:
    case InvalidSyntax(node: Node)            extends EvalutationError
    case UndefinedIdentifier(name: String)    extends EvalutationError
    case UncallableValue(value: MonkeyObject) extends EvalutationError
    case IndexOutOfBounds(item: MonkeyObject, index: MonkeyObject)
        extends EvalutationError
    case ItemNotSubscriptable(item: MonkeyObject) extends EvalutationError
    case InvalidIndexValue(index: MonkeyObject)   extends EvalutationError

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

  import Environment.{ extend, find }

  private def eval(
      node: Node,
      env: Environment
  ): Either[EvalutationError, (MonkeyObject, Environment)] =
    node match
      case Expression.IntegerLiteral(value) =>
        Right(MonkeyObject.IntegerLiteral(value), env)
      case Expression.BooleanLiteral(value) =>
        Right(MonkeyObject.of(value), env)
      case Expression.StringLiteral(value) =>
        Right(MonkeyObject.StringLiteral(value), env)
      case Expression.ArrayLiteral(items) =>
        evalArray(items, env).map: result =>
          (result, env)
      case Expression.Identifier(identifier) =>
        env.find(identifier) match
          case None        => Left(EvalutationError.UndefinedIdentifier(identifier))
          case Some(value) => Right(value, env)
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
      case Expression.Func(params, body) =>
        Right(MonkeyObject.FunctionLiteral(params, body, env), env)
      case Expression.Call(func, args) =>
        evalFunctionCall(func, args, env)
      case Expression.Index(left, index) =>
        evalIndex(left, index, env)

  private def evalIndex(left: Expression, index: Node, env: Environment) =
    eval(left, env).flatMap: (leftVal, _) =>
      eval(index, env).flatMap: (indexVal, _) =>
        (leftVal, indexVal) match
          case (
                xs @ MonkeyObject.ArrayLiteral(items),
                index @ MonkeyObject.IntegerLiteral(i)
              ) =>
            if 0 <= i && i < items.length
            then Right(items(i), env)
            else Left(EvalutationError.IndexOutOfBounds(xs, index))
          case (MonkeyObject.ArrayLiteral(_), index) =>
            Left(EvalutationError.InvalidIndexValue(index))
          case (item, _) =>
            Left(EvalutationError.ItemNotSubscriptable(item))

  private def evalArray(items: Array[Expression], env: Environment) =
    def doEval(
        items: List[Expression],
        acc: List[MonkeyObject]
    ): Either[EvalutationError, MonkeyObject.ArrayLiteral] =
      items match
        case Nil => Right(MonkeyObject.ArrayLiteral(acc.reverse.toArray))
        case item :: rest => eval(item, env).flatMap: (item, _) =>
            doEval(rest, item :: acc)

    doEval(items.toList, List.empty)

  private def evalFunctionCall(
      func: Expression,
      args: List[Expression],
      env: Environment
  ) =
    eval(func, env).flatMap: (f, _) =>
      f match
        case MonkeyObject.FunctionLiteral(params, body, funcEnv) =>
          evalCallArguments(args, env).flatMap: args =>
            val extendedEnv = Environment.of(params, args)
              .extend(funcEnv)
              .extend(env)
            eval(body, extendedEnv).map: (result, _) =>
              (result, env)
        case other => Left(EvalutationError.UncallableValue(other))

  private def evalCallArguments(
      args: List[Expression],
      env: Environment
  ): Either[EvalutationError, List[MonkeyObject]] =
    evalCallArguments(args, env, List.empty)

  @annotation.tailrec
  private def evalCallArguments(
      args: List[Expression],
      env: Environment,
      acc: List[MonkeyObject]
  ): Either[EvalutationError, List[MonkeyObject]] =
    args match
      case Nil => Right(acc.reverse)
      case arg :: rest =>
        eval(arg, env) match
          case Left(value)     => Left(value)
          case Right(value, _) => evalCallArguments(rest, env, value :: acc)

  private def evalBlockStatement(
      nodes: List[Node],
      env: Environment
  ): Either[EvalutationError, MonkeyObject] =
    evalBlockStatement(nodes, env, None).map(_.getOrElse(MonkeyObject.Null))

  @annotation.tailrec
  def evalBlockStatement(
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
          case Right(result, env) => evalBlockStatement(rest, env, Some(result))

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
      case (
            Right(MonkeyObject.StringLiteral(leftVal), _),
            Right(MonkeyObject.StringLiteral(rightVal), _)
          ) =>
        Right(MonkeyObject.StringLiteral(leftVal + rightVal))
      case _ => Left(EvalutationError.InvalidSyntax(node))
