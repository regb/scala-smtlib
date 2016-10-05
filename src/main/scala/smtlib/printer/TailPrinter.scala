package smtlib
package printer

import common.LinkedList

import parser.Commands._
import parser.CommandsResponses._
import parser.Terms._

import java.io.Writer

import scala.collection.mutable.Stack

object TailPrinter extends Printer {

  override val name: String = "tail-printer"

  override protected def newContext(writer: Writer) = new TailContext(writer)

  class TailContext(writer: Writer) extends PrintingContext(writer) {
    var actions = new LinkedList[() => Unit]
    val actionStack = new Stack[LinkedList[() => Unit]]

    override protected def print(tree: Tree): Unit = {
      actions.append(() => super.print(tree))
    }

    override protected def print(str: String): Unit = {
      actions.append(() => super.print(str))
    }

    override protected def finish(): Unit = {
      while (!actions.isEmpty || !actionStack.isEmpty) {
        if (actions.isEmpty) {
          actions = actionStack.pop()
        } else {
          val action = actions.pop()
          actionStack.push(actions)
          actions = new LinkedList[() => Unit]
          action()
        }
      }
    }
  }
}
