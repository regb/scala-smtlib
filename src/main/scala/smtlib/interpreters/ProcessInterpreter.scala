package smtlib
package interpreters

import lexer.Lexer
import parser.Parser
import parser.Terms._
import parser.Commands._
import parser.CommandsResponses._
import printer._

import java.io._

abstract class ProcessInterpreter(protected val process: Process) extends Interpreter {

  def this(executable: String, args: Array[String]) = {
    this(java.lang.Runtime.getRuntime.exec((executable :: args.toList).mkString(" ")))
  }


  lazy val in = new BufferedWriter(new OutputStreamWriter(process.getOutputStream))
  lazy val out = new BufferedReader(new InputStreamReader(process.getInputStream))

  lazy val parser = new Parser(new Lexer(out))

  def parseResponseOf(cmd: SExpr): SExpr = cmd match {
    case CheckSat() => parser.parseCheckSatResponse
    case GetAssertions() => parser.parseGetAssertionsResponse
    case GetUnsatCore() => parser.parseGetUnsatCoreResponse
    case GetProof() => parser.parseGetProofResponse
    case GetValue(_, _) => parser.parseGetValueResponse
    case GetAssignment() => parser.parseGetAssignmentResponse

    case GetOption(_) => parser.parseGetOptionResponse
    case GetInfo(_) => parser.parseGetInfoResponse

    case GetModel() => parser.parseGetModelResponse

    case (_: Command) => parser.parseGenResponse

    //in the case the input was not a known command, we assume nothing and 
    //parse an arbitrary s-expr
    case _ => parser.parseSExpr
  }

  /*
   * eval is blocking, and not synchronized. You
   * should not invoke eval from different threads.
   */
  override def eval(cmd: SExpr): SExpr = {
    try {
      RecursivePrinter.printSExpr(cmd, in)
      in.write("\n")
      in.flush

      parseResponseOf(cmd)
    } catch {
      case (ex: Exception) => {
        if(cmd == CheckSat()) CheckSatStatus(UnknownStatus)
        else Error("Solver encountered exception: " + ex)
      }
    }
  }

  private var isKilled = false

  override def free(): Unit = synchronized {
    if(!isKilled) {
      try {
        RecursivePrinter.printCommand(Exit(), in)
        in.write("\n")
        in.flush

        process.destroy()
        in.close()
      } catch {
        case (io: java.io.IOException) => ()
      } finally {
        isKilled = true
        try { in.close() } catch { case (io: java.io.IOException) => () }
      }
    }
  }

  def kill(): Unit = synchronized {
    if(!isKilled) {
      try {
        process.destroyForcibly()
        in.close()
      } catch {
        case (io: java.io.IOException) => ()
      } finally {
        isKilled = true
      }
    }
  }

  override def interrupt(): Unit = synchronized {
    kill()
  }
}
