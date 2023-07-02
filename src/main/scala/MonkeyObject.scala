enum MonkeyObject:
  case IntegerLiteral(value: Int)     extends MonkeyObject
  case BooleanLiteral(value: Boolean) extends MonkeyObject
  case Null                           extends MonkeyObject
