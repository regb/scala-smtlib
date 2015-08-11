package smtlib
package interpreters

import parser.Commands._
import parser.CommandsResponses._
import printer.RecursivePrinter

class CVC4Interpreter(executable: String, args: Array[String]) extends ProcessInterpreter(executable, args) {

  RecursivePrinter.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

  override def parseResponseOf(cmd: Command): CommandResponse = cmd match {
    case DefineFunsRec(funs, bodies) =>
      // CVC4 translates definefunsrec in three commands per function,
      // and thus emits 3x (success)
      val res = for (i <- 1 to funs.size*3) yield {
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
                     "--produce-models",
                     "--no-incremental",
                     "--tear-down-incremental",
                     "--dt-rewrite-error-sel",
                     "--print-success",
                     "--lang", "smt2.5")
    new CVC4Interpreter(executable, args)
  }

}
