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
        value match
          case true  => Right(TRUE)
          case false => Right(FALSE)
      case Statement.Expr(expr) => eval(expr)
      case Expression.PrefixOperator(Token.Bang, expr) =>
        eval(expr).flatMap:
          case TRUE                           => Right(FALSE)
          case FALSE                          => Right(TRUE)
          case MonkeyObject.IntegerLiteral(0) => Right(TRUE)
          case MonkeyObject.IntegerLiteral(_) => Right(FALSE)
          case MonkeyObject.Null              => Right(TRUE)
          case _                              => Left(EvalutationError.InvalidSyntax(node))
      case Expression.PrefixOperator(Token.Minus, expr) =>
        eval(expr).flatMap:
          case MonkeyObject.IntegerLiteral(value) =>
            Right(MonkeyObject.IntegerLiteral(-value))
          case _ => Left(EvalutationError.InvalidSyntax(node))

  private val TRUE  = MonkeyObject.BooleanLiteral(true)
  private val FALSE = MonkeyObject.BooleanLiteral(false)
