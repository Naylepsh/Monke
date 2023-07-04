enum MonkeyObject:
  case IntegerLiteral(value: Int)     extends MonkeyObject
  case BooleanLiteral(value: Boolean) extends MonkeyObject
  case FunctionLiteral(
      parameters: List[AST.Expression.Identifier],
      block: AST.Statement.Block,
      env: Environment
  )                                     extends MonkeyObject
  case Null                             extends MonkeyObject
  case ReturnValue(value: MonkeyObject) extends MonkeyObject

object MonkeyObject:
  def of(value: Boolean): MonkeyObject =
    if value then TRUE else FALSE

  def isTruthy(value: MonkeyObject): Boolean =
    value match
      case MonkeyObject.BooleanLiteral(value) => value
      case MonkeyObject.IntegerLiteral(0)     => false
      case MonkeyObject.IntegerLiteral(_)     => true
      case MonkeyObject.Null                  => false
      case _                                  => true

  private val TRUE  = MonkeyObject.BooleanLiteral(true)
  private val FALSE = MonkeyObject.BooleanLiteral(false)

  given showMonkey(using showNode: Show[AST.Node]): Show[MonkeyObject] with
    def show(o: MonkeyObject): String =
      o match
        case IntegerLiteral(value) => value.toString
        case BooleanLiteral(value) => value.toString
        case Null                  => "null"
        case ReturnValue(value)    => s"return ${show(value)}"
        case FunctionLiteral(params, body, _) =>
          val ps = params.map(showNode.show).mkString(", ")
          s"fn($ps) ${showNode.show(body)}"
