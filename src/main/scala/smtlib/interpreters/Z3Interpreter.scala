package smtlib
package interpreters

import Commands._
import CommandResponses._

//import scala.sys.process._
import java.io._

class Z3Interpreter extends Interpreter {


  //private val pio = new ProcessIO(
  //  in => z3In = new BufferedWriter(new OutputStreamWriter(in)),
  //  out => z3Out = new BufferedReader(new InputStreamReader(out)),
  //  err => ()
  //)

  //val z3 = "z3 -in -smt2".run(pio)
  private val z3 = new ProcessBuilder("z3", "-in", "-smt2").redirectErrorStream(true).start

  //var z3In: Writer = null
  //var z3Out: Reader = null
  val z3In = new BufferedWriter(new OutputStreamWriter(z3.getOutputStream))
  val z3Out = new BufferedReader(new InputStreamReader(z3.getInputStream))

  PrettyPrinter(SetOption(PrintSuccess(true)), z3In)
  z3In.write("\n")
  z3In.flush

  val parser = new ResponseParser(z3Out)
  parser.next

  override def eval(cmd: Command): CommandResponse = {
    PrettyPrinter(cmd, z3In)
    z3In.write("\n")
    z3In.flush
    parser.next
  }

  override def free(): Unit = {
    z3.destroy
    z3In.close
  }

}
