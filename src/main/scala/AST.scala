object AST:
  enum Expression:
    case Identifier(value: String)      extends Expression
    case IntegerLiteral(value: Int)     extends Expression
    case BooleanLiteral(value: Boolean) extends Expression
    case PrefixOperator(token: Token, expression: Expression)
        extends Expression
    case InfixOperator(left: Expression, token: Token, right: Expression)
        extends Expression
    case If(
        condition: Expression,
        consequence: Statement.Block,
        alternative: Option[Statement.Block]
    ) extends Expression

  object Expression:
    given showExpression(
        using showToken: Show[Token],
        // showStatement: Show[Statement]
    ): Show[Expression] with
      def show(expr: Expression): String =
        expr match
          case Identifier(value)     => value
          case IntegerLiteral(value) => value.toString
          case BooleanLiteral(value) => value.toString
          case PrefixOperator(token, expression) =>
            s"(${showToken.show(token)}${show(expression)})"
          case InfixOperator(left, token, right) =>
            s"(${show(left)} ${showToken.show(token)} ${show(right)})"
          // case If(condition, consequence, None) =>
          //   s"(${show(condition)}) ${showStatement.show(consequence)}"
          // case If(condition, consequence, Some(alternative)) =>
          //   s"(${show(condition)}) ${showStatement.show(consequence)} else ${showStatement.show(alternative)}"

  enum Statement:
    case Let(identifier: String, expression: Expression) extends Statement
    case Return(expression: Expression)                  extends Statement
    case Expr(expression: Expression)                    extends Statement
    case Block(nodes: List[Expression | Statement])      extends Statement
  object Statement:
    given showStatement(
        using showExpression: Show[Expression],
        showToken: Show[Token]
    ): Show[Statement] with
      def show(statement: Statement): String =
        statement match
          case Let(identifier, expression) =>
            s"let $identifier = ${showExpression.show(expression)}"
          case Return(expression) =>
            s"return ${showExpression.show(expression)}"
          case Expr(expression) => showExpression.show(expression)
          case Block(nodes) =>
            nodes
              .map:
                case expression: Expression => showExpression.show(expression)
                case statement: Statement   => show(statement)
              .mkString("{", "\n", "}")

  type Node = Expression | Statement

  case class Program(nodes: List[Node])
  object Program:
    given showProgram(
        using showStatement: Show[Statement],
        showExpression: Show[Expression]
    ): Show[Program] with
      def show(program: Program): String =
        program
          .nodes
          .map: node =>
            node match
              case statement: Statement   => showStatement.show(statement)
              case expression: Expression => showExpression.show(expression)
          .mkString("\n")
