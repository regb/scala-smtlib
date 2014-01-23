package smtlib

import Commands.{Script, Command}
import CommandResponses.CommandResponse

/*
 * An interpreter is a stateful object that can eval Commands and returns
 * CommandResponse.
 */

trait Interpreter {

  def eval(cmd: Command): CommandResponse

}

object Interpreter {

  import java.io.Reader

  def execute(script: Script)(implicit interpreter: Interpreter): Unit = {
    for(cmd <- script.commands)
      interpreter.eval(cmd)
  }

  def execute(scriptReader: Reader)(implicit interpreter: Interpreter): Unit = {
    val parser = new Parser(scriptReader)
    execute(Script(parser))
  }

}
