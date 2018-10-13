package smtlib
package interpreters

import trees.Terms._
import trees.Commands._
import trees.CommandsResponses._

class CVC4Interpreter(executable: String, args: Array[String], tailPrinter: Boolean = false)
  extends ProcessInterpreter(executable, args, tailPrinter) {

  printer.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

  override def parseResponseOf(cmd: SExpr): SExpr = cmd match {
    case DefineFunsRec(funs, _) =>
      // CVC4 translates define-funs-rec in three commands per function,
      // and thus emits 3x success.
      val res = for (i <- 1 to funs.size*3) yield {
        parser.parseGenResponse
      }

      res.find(_ != Success).getOrElse(Success)

    case DefineFunRec(_) =>
      // CVC4 translates define-fun-rec in three commands,
      // and thus emits 3x success.
      val res = for (i <- 1 to 3) yield {
        parser.parseGenResponse
      }

      res.find(_ != Success).getOrElse(Success)

    case _ =>
      super.parseResponseOf(cmd)
  }
}

object CVC4Interpreter {

  def buildDefault: CVC4Interpreter = {
    val executable = "cvc4"
    val args = Array("-q",
                     "-i",
                     "--produce-models",
                     "--dt-rewrite-error-sel",
                     "--print-success",
                     "--lang", "smt2.5")
    new CVC4Interpreter(executable, args)
  }

}
