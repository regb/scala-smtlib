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
    in => z3In = new PrintWriter(in),
    out => {
      z3Out = new InputStreamReader(out)
      //new Thread {
      //  override def run() {
      //    while(true)
      //      println(z3Out.read.toChar)
      //  }
      //}.start
    },
    err => ()
  )

  //val z3 = "z3 -in -smt".run(pio)
  val cvc = "cvc4 -lang=smtlib".run(pio)

  //PrettyPrinter(SetOption(PrintSuccess(true)), z3In)
  //z3In.write("\n")
  //PrettyPrinter(SetOption(PrintSuccess(true)), z3In)
  //z3In.write("\n")
  //println("Z3 output: " + readFromZ3)

  private def readFromZ3: String = {
    var res: String = ""
    while(z3Out.ready) {
      val c = z3Out.read
      res += c.toChar
    }
    res
  }

  def eval(cmd: Command) = ???

}
