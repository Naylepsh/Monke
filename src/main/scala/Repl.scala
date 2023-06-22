import scala.io.StdIn.readLine

object Repl:
  @annotation.tailrec
  def run: Unit =
    val line   = readLine(">>")
    val tokens = Lexer.tokenize(line)
    tokens.foreach(println)
    run
