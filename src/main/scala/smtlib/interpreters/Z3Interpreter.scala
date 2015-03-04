package smtlib
package interpreters

import lexer.Lexer
import parser.Parser
import parser.Commands._
import parser.CommandsResponses._
import printer._

//import scala.sys.process._
import java.io._

class Z3Interpreter(executable: String = "z3") extends ProcessInterpreter {

  protected override val process =
    new ProcessBuilder(executable,
                        "-in",
                        "-smt2").redirectErrorStream(true).start

  RecursivePrinter.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

}

class Z3InterpreterV3 extends ProcessInterpreter {

  protected override val process = new ProcessBuilder("z3", "-in", "-m", "-smt2").redirectErrorStream(true).start

  RecursivePrinter.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

}
