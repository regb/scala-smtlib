package smtlib
package interpreters

import Commands._
import CommandResponses._

//import scala.sys.process._
import java.io._

class CVC4Interpreter extends Interpreter {

  private val cvc4 = new ProcessBuilder("cvc4",
                                                "-q",
                                                "--produce-models",
                                                "--no-incremental",
                                                "--tear-down-incremental",
                                                "--print-success",
                                                "--lang", "smt").redirectErrorStream(true).start

  val cvc4In = new BufferedWriter(new OutputStreamWriter(cvc4.getOutputStream))
  val cvc4Out = new BufferedReader(new InputStreamReader(cvc4.getInputStream))

  val parser = new ResponseParser(cvc4Out)

  override def eval(cmd: Command): CommandResponse = {
    PrettyPrinter(cmd, cvc4In)
    cvc4In.write("\n")
    cvc4In.flush
    parser.next
  }

  override def free(): Unit = {
    PrettyPrinter(Exit, cvc4In)
    cvc4In.write("\n")
    cvc4In.flush

    cvc4.destroy
    cvc4In.close
  }
}
