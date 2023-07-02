import AST.*

object Eval:

  def eval(program: AST.Program): MonkeyObject =
    @annotation.tailrec
    def doEval(
        nodes: List[Node],
        result: Option[MonkeyObject]
    ): Option[MonkeyObject] =
      nodes match
        case Nil          => result
        case node :: rest => doEval(rest, Some(eval(node)))

    doEval(program.nodes, None).getOrElse(MonkeyObject.Null)

  @annotation.tailrec
  def eval(node: Node): MonkeyObject =
    node match
      case Expression.IntegerLiteral(value) =>
        MonkeyObject.IntegerLiteral(value)
      case Expression.BooleanLiteral(value) =>
        MonkeyObject.BooleanLiteral(value)
      case Statement.Expr(expr) => eval(expr)
