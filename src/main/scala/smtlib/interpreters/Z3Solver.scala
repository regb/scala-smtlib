package smtlib
package interpreters

import Commands._
import CommandResponses._

import scala.sys.process._
import java.io._

class Z3Interpreter extends Interpreter {

  var z3In: Writer = null
  var z3Out: Reader = null

  private val pio = new ProcessIO(
    in => z3In = new BufferedWriter(new OutputStreamWriter(in)),
    out => z3Out = new BufferedReader(new InputStreamReader(out)),
    err => ()
  )

  val z3 = "z3 -in -smt2".run(pio)


  PrettyPrinter(SetOption(PrintSuccess(true)), z3In)
  z3In.write("\n")
  z3In.flush

  val parser = new ResponseParser(z3Out)
  parser.next

  def readFromZ3: String = {
    z3In.flush
    var res: String = ""
    while(z3Out.ready) {
      val c = z3Out.read
      res += c.toChar
    }
    res
  }

  def eval(cmd: Command): CommandResponse = {
    PrettyPrinter(cmd, z3In)
    z3In.write("\n")
    z3In.flush
    parser.next
  }

}
