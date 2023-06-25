enum Token:
  case Illegal extends Token
  case EOF     extends Token
  // Items
  case Identifier(value: String) extends Token
  case Integer(value: String)    extends Token
  // Operators
  case Assign      extends Token
  case Plus        extends Token
  case Minus       extends Token
  case Bang        extends Token
  case Asterisk    extends Token
  case Slash       extends Token
  case Equal       extends Token
  case NotEqual    extends Token
  case GreaterThan extends Token
  case LesserThan  extends Token
  // Delimiters
  case Comma      extends Token
  case Semicolon  extends Token
  case LeftParen  extends Token
  case RightParen extends Token
  case LeftBrace  extends Token
  case RightBrace extends Token
  // Keywords
  case Func   extends Token
  case Let    extends Token
  case True   extends Token
  case False  extends Token
  case If     extends Token
  case Else   extends Token
  case Return extends Token

object Token:
  given showToken: Show[Token] with
    def show(token: Token): String =
      token match
        case Illegal                   => "Illegal"
        case EOF                       => ""
        case Identifier(value: String) => value
        case Integer(value: String)    => value
        case Assign                    => "="
        case Plus                      => "+"
        case Minus                     => "-"
        case Bang                      => "!"
        case Asterisk                  => "*"
        case Slash                     => "/"
        case Equal                     => "=="
        case NotEqual                  => "!="
        case GreaterThan               => ">"
        case LesserThan                => "<"
        case Comma                     => ","
        case Semicolon                 => ";"
        case LeftParen                 => "("
        case RightParen                => ")"
        case LeftBrace                 => "{"
        case RightBrace                => "}"
        case Func                      => "fn"
        case Let                       => "let"
        case True                      => "true"
        case False                     => "false"
        case If                        => "if"
        case Else                      => "else"
        case Return                    => "return"
