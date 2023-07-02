enum MonkeyObject:
  case IntegerLiteral(value: Int)     extends MonkeyObject
  case BooleanLiteral(value: Boolean) extends MonkeyObject
  case Null                           extends MonkeyObject

object MonkeyObject:
  def of(value: Boolean): MonkeyObject =
    if value then TRUE else FALSE

  def isTruthy(value: MonkeyObject): Boolean =
    value match
      case MonkeyObject.BooleanLiteral(value) => value
      case MonkeyObject.IntegerLiteral(0)     => false
      case MonkeyObject.IntegerLiteral(_)     => true
      case MonkeyObject.Null                  => false

  private val TRUE  = MonkeyObject.BooleanLiteral(true)
  private val FALSE = MonkeyObject.BooleanLiteral(false)

  given Show[MonkeyObject] with
    def show(o: MonkeyObject): String =
      o match
        case IntegerLiteral(value) => value.toString
        case BooleanLiteral(value) => value.toString
        case Null                  => "null"
