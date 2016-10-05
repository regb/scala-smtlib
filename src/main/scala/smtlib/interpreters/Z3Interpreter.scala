package smtlib
package interpreters

import parser.Commands._
import printer.RecursivePrinter

class Z3Interpreter(executable: String, args: Array[String]) extends ProcessInterpreter(executable, args) {

  printer.printCommand(SetOption(PrintSuccess(true)), in)
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
