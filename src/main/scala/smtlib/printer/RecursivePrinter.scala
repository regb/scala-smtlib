package smtlib
package printer

import parser.Commands._
import parser.CommandsResponses._
import parser.Terms._
import parser.Tree

import java.io.Writer

object RecursivePrinter extends Printer {

  override val name: String = "recursive-printer"

  override protected def newContext(writer: Writer): PrintingContext = new PrintingContext(writer)
}
