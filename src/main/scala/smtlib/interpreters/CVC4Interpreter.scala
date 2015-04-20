package smtlib
package interpreters

import lexer.Lexer
import parser.Parser
import parser.Commands._
import parser.CommandsResponses._
import printer._

//import scala.sys.process._
import java.io._

class CVC4Interpreter(executable: String, args: Array[String]) extends ProcessInterpreter(executable, args) {

  RecursivePrinter.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

  override def eval(cmd: Command): CommandResponse = {
    try {
      RecursivePrinter.printCommand(cmd, in)
      in.write("\n")
      in.flush
      cmd match {
        case CheckSat() => parser.parseCheckSatResponse
        case GetAssertions() => parser.parseGetAssertionsResponse
        case GetUnsatCore() => parser.parseGetUnsatCoreResponse
        case GetProof() => parser.parseGetProofResponse
        case GetValue(_, _) => parser.parseGetValueResponse
        case GetAssignment() => parser.parseGetAssignmentResponse

        case GetOption(_) => parser.parseGetOptionResponse
        case GetInfo(_) => parser.parseGetInfoResponse

        case GetModel() => parser.parseGetModelResponse

        case DefineFunsRec(funs, bodies) =>
          // CVC4 translates definefunsrec in two commands per function,
          // and thus emits 2x (success)
          val res = for (i <- 1 to funs.size*2) yield {
            parser.parseGenResponse
          }

          res.find(_ != Success).getOrElse(Success)

        case _ => parser.parseGenResponse
      }
    } catch {
      case (ex: Exception) => {
        if(cmd == CheckSat()) CheckSatStatus(UnknownStatus)
        else Error("Solver encountered exception: " + ex)
      }
    }
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
                     "--lang", "smt")
    new CVC4Interpreter(executable, args)
  }

}
