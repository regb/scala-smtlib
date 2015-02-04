package smtlib
package interpreters

import lexer.Lexer
import parser.Parser
import parser.Commands._
import parser.CommandsResponses._
import printer._

//import scala.sys.process._
import java.io._

class CVC4Interpreter extends ProcessInterpreter {

  protected override val process = 
    new ProcessBuilder("cvc4",
                          "-q",
                          "--produce-models",
                          "--no-incremental",
                          "--tear-down-incremental",
                          "--dt-rewrite-error-sel",
                          "--print-success",
                          "--lang", "smt").redirectErrorStream(true).start

  RecursivePrinter.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

}
