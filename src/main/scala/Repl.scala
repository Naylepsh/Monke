import scala.io.StdIn.readLine
import Show.*
import AST.Node.given
import MonkeyObject.given

object Repl:
  @annotation.tailrec
  def run: Unit =
    val line   = readLine(">>")
    val tokens = Lexer.tokenize(line)
    val ast    = Parser.parse(tokens)

    ast match
      case Left(errors) => errors.foreach(println)
      case Right(program) =>
        Eval.eval(program) match
          case Left(error)  => println(error)
          case Right(value) => println(value.show)

    run
