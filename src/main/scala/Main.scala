@main def hello: Unit =
  val input = """!-/*5;
  |5 < 10 > 5;
  |
  |if (5 < 10) {
  |  return true;
  |} else {
  |  return false;
  |}
  |
  |10 == 10;
  |10 != 9;""".stripMargin

  val tokens = Lexer.tokenize(input)
  println(tokens)
