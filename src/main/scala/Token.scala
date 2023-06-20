enum Token:
  case Illegal                   extends Token
  case EOF                       extends Token
  case Identifier(value: String) extends Token
  case Integer(value: String)    extends Token
  case Assign                    extends Token
  case Plus                      extends Token
  case Comma                     extends Token
  case Semicolon                 extends Token
  case LeftParen                 extends Token
  case RightParen                extends Token
  case LeftBrace                 extends Token
  case RightBrace                extends Token
  case Func                      extends Token
  case Let                       extends Token
