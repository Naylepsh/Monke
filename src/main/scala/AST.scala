object AST:
  enum Expression:
    case Identifier(value: String)              extends Expression
    case IntegerLiteral(value: Int)             extends Expression
    case BooleanLiteral(value: Boolean)         extends Expression
    case StringLiteral(value: String)           extends Expression
    case ArrayLiteral(items: Array[Expression]) extends Expression
    case PrefixOperator(token: Token, expression: Expression)
        extends Expression
    case InfixOperator(left: Expression, token: Token, right: Expression)
        extends Expression
    case If(
        condition: Expression,
        consequence: Statement.Block,
        alternative: Option[Statement.Block]
    ) extends Expression
    case Func(parameters: List[Identifier], body: Statement.Block)
        extends Expression
    case Call(function: Expression, arguments: List[Expression])
        extends Expression
    case Index(left: Expression, index: Expression | Statement)
        extends Expression

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement
    case Return(expression: Expression)                  extends Statement
    case Expr(expression: Expression)                    extends Statement
    case Block(nodes: List[Expression | Statement])      extends Statement

  type Node = Expression | Statement
  object Node:
    given showNode(using showToken: Show[Token]): Show[Node] with
      def show(node: Node): String =
        node match
          case Expression.Identifier(value)     => value
          case Expression.IntegerLiteral(value) => value.toString
          case Expression.BooleanLiteral(value) => value.toString
          case Expression.StringLiteral(value)  => value
          case Expression.ArrayLiteral(items) =>
            items.map(show).mkString("[", ",", "]")
          case Expression.PrefixOperator(token, expression) =>
            s"(${showToken.show(token)}${show(expression)})"
          case Expression.InfixOperator(left, token, right) =>
            s"(${show(left)} ${showToken.show(token)} ${show(right)})"
          case Expression.If(condition, consequence, None) =>
            s"(${show(condition)}) ${show(consequence)}"
          case Expression.If(condition, consequence, Some(alternative)) =>
            s"(${show(condition)}) ${show(consequence)} else ${show(alternative)}"
          case Expression.Func(parameters, body) =>
            val params = parameters.map(show).mkString(", ")
            s"fn($params) ${show(body)}"
          case Expression.Call(func, arguments) =>
            val args = arguments.map(show).mkString(", ")
            s"${show(func)}($args)"
          case Expression.Index(left, index) =>
            s"${show(left)}[${show(index)}]"
          case Statement.Let(identifier, expression) =>
            s"let $identifier = ${show(expression)}"
          case Statement.Return(expression) =>
            s"return ${show(expression)}"
          case Statement.Expr(expression) => show(expression)
          case Statement.Block(nodes) =>
            nodes.map(show).mkString("{ ", "\n", " }")

  case class Program(nodes: List[Node])
  object Program:
    given showProgram(using showNode: Show[Node]): Show[Program] with
      def show(program: Program): String =
        program.nodes.map(showNode.show).mkString("\n")
