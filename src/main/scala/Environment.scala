type Environment = List[(String, MonkeyObject)]

object Environment:
  def empty: Environment = List.empty

  def of(
      params: List[AST.Expression.Identifier],
      args: List[MonkeyObject]
  ): Environment =
    params.map(_.value).zip(args)

  extension (env: Environment)
    def find(name: String): Option[MonkeyObject] =
      env
        .find: (identifierName, _) =>
          identifierName == name
        .map: (_, value) =>
          value

    def extend(outer: Environment): Environment = env ::: outer
