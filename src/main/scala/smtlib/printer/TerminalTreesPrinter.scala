package smtlib
package printer

import parser.Commands._
import parser.CommandsResponses._
import parser.Terms._

import java.io.Writer

trait TerminalTreesPrinter {

  /*
   * Provide functions to print the basic elements of the trees.
   * Only print the simplest elements, that do not need a recursion.
   */

  protected def printConstant(c: Constant, writer: Writer): Unit = c match {
    case SNumeral(value) => writer.write(value.toString)
    case SHexadecimal(value) => writer.write(value.toString)
    case SBinary(value) => writer.write("#b" + value.map(if(_) "1" else "0").mkString)
    case SDecimal(value) => writer.write(value.toString)
    case SString(value) =>
      writer.write("\"")
      writer.write(value.flatMap(c => if(c == '"') "\\\"" else List(c)))
      writer.write("\"")
  }


  protected def printId(id: Identifier, writer: Writer): Unit = {
    if(id.indices.isEmpty)
      writer.write(id.symbol.name)
    else {
      writer.write("(_ ")
      writer.write(id.symbol.name)
      writer.write(' ')
      writer.write(id.indices.head.toString)
      id.indices.tail.foreach(n => writer.write(" " + n.toString))
      writer.write(")")
    }
  }

  protected def printLogic(logic: Logic, writer: Writer): Unit = logic match {
    case QF_UF => 
      writer.write("QF_UF")
    case QF_LRA => 
      writer.write("QF_LRA")
    case QF_AX => 
      writer.write("QF_AX")
    case QF_A => 
      writer.write("QF_A")
    case Undef => ???
  }

  protected def printKeyword(keyword: SKeyword, writer: Writer): Unit = {
    writer.write(":")
    writer.write(keyword.name)
  }

  protected def printInfoFlag(flag: InfoFlag, writer: Writer): Unit = flag match {
    case ErrorBehaviourInfoFlag => 
      writer.write(":error-behaviour")
    case NameInfoFlag => 
      writer.write(":name")
    case AuthorsInfoFlag => 
      writer.write(":authors")
    case VersionInfoFlag => 
      writer.write(":version")
    case StatusInfoFlag => 
      writer.write(":status")
    case ReasonUnknownInfoFlag => 
      writer.write(":reason-unknonwn")
    case AllStatisticsInfoFlag => 
      writer.write(":all-statistics")
    case KeywordInfoFlag(keyword) =>
      writer.write(':')
      writer.write(keyword)
  }

}
