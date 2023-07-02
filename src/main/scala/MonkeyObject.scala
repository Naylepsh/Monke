enum MonkeyObject:
  case IntegerLiteral(value: Int)     extends MonkeyObject
  case BooleanLiteral(value: Boolean) extends MonkeyObject
  case Null                           extends MonkeyObject

object MonkeyObject:
  given Show[MonkeyObject] with
    def show(o: MonkeyObject): String =
      o match
        case IntegerLiteral(value) => value.toString
        case BooleanLiteral(value) => value.toString
        case Null                  => "null"
