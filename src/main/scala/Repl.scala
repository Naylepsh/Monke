import scala.io.StdIn.readLine
import Show.*
import AST.Node.given
import MonkeyObject.given

object Repl:
  def run: Unit = run(Eval.Environment.empty)

  @annotation.tailrec
  private def run(env: Eval.Environment): Unit =
    val line   = readLine(">>")
    val tokens = Lexer.tokenize(line)
    val ast    = Parser.parse(tokens)

    ast match
      case Left(errors) =>
        errors.foreach(println)
        run(env)
      case Right(program) =>
        Eval.eval(program, env) match
          case Left(error) =>
            println(error)
            run(env)
          case Right(value, env) =>
            println(value.show)
            run(env)
