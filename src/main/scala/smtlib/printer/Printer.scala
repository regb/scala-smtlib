package smtlib
package printer

import parser.Commands._
import parser.CommandsResponses._
import parser.Terms._

import java.io.Writer
import java.io.StringWriter
import java.io.BufferedWriter

trait Printer {

  def printScript(script: Script, writer: Writer): Unit
  def printCommand(command: Command, writer: Writer): Unit
  def printTerm(term: Term, writer: Writer): Unit
  def printSort(sort: Sort, writer: Writer): Unit
  def printCommandResponse(response: CommandResponse, writer: Writer): Unit

  def toString(script: Script): String = {
    val output = new StringWriter
    val sWriter = new BufferedWriter(output)
    printScript(script, sWriter)
    sWriter.flush
    output.toString
  }

  def toString(command: Command): String = {
    val output = new StringWriter
    val sWriter = new BufferedWriter(output)
    printCommand(command, sWriter)
    sWriter.flush
    output.toString
  }

  def toString(term: Term): String = {
    val output = new StringWriter
    val sWriter = new BufferedWriter(output)
    printTerm(term, sWriter)
    sWriter.flush
    output.toString
  }

  def toString(sort: Sort): String = {
    val output = new StringWriter
    val sWriter = new BufferedWriter(output)
    printSort(sort, sWriter)
    sWriter.flush
    output.toString
  }

  def toString(response: CommandResponse): String = {
    val output = new StringWriter
    val sWriter = new BufferedWriter(output)
    printCommandResponse(response, sWriter)
    sWriter.flush
    output.toString
  }

}
