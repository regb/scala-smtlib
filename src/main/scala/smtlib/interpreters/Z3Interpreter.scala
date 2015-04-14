package smtlib
package interpreters

import lexer.Lexer
import parser.Parser
import parser.Commands._
import parser.CommandsResponses._
import printer._

//import scala.sys.process._
import java.io._

class Z3Interpreter(executable: String, args: Array[String]) extends ProcessInterpreter(executable, args) {

  RecursivePrinter.printCommand(SetOption(PrintSuccess(true)), in)
  in.write("\n")
  in.flush
  parser.parseGenResponse

}

object Z3Interpreter {

  def buildDefault: Z3Interpreter = {
    val executable = "z3"
    val args = Array("-in", "-smt2")
    new Z3Interpreter(executable, args)
  }

  def buildForV3: Z3Interpreter = {
    val executable = "z3"
    val args = Array("-in", "-m", "-smt2")
    new Z3Interpreter(executable, args)
  }

}
