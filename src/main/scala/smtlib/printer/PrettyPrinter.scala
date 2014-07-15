package smtlib
package printer

import parser.Commands._
import parser.Terms._

import java.io.Writer
import java.io.StringWriter
import java.io.BufferedWriter

object PrettyPrinter {

  def toString(script: Script): String = {
    val sWriter = new BufferedWriter(new StringWriter)
    printScript(script, sWriter)
    sWriter.toString
  }

  def toString(command: Command): String = {
    val sWriter = new BufferedWriter(new StringWriter)
    printCommand(command, sWriter)
    sWriter.toString
  }

  def printScript(script: Script, writer: Writer): Unit = ???

  def printCommand(command: Command, writer: Writer): Unit = command match {
    case SetLogic(logic) => {
      writer.write("(set-logic ")
      printLogic(logic, writer)
      writer.write(")\n")
    }
    case SetOption(option) => {
      writer.write("(set-option ")
      printOption(option, writer)
      writer.write(")\n")
    }
    case SetInfo(attribute) => {
      writer.write("(set-info ")
      printAttribute(attribute, writer)
      writer.write(")\n")
    }
    case DeclareSort(name, arity) => {
      writer.write("(declare-sort ")
      writer.write(name.name)
      writer.write(arity.toInt)
      writer.write(")\n")
    }
    case DefineSort(name, params, sort) => {
      writer.write("(define-sort ")
      writer.write(name.name)
      writer.write(params.map(_.name).mkString("(", " ", ")"))
      printSort(sort, writer)
      writer.write(")\n")
    }
    case DeclareFun(name, paramSorts, returnSort) => ???
    //  SList(SSymbol("declare-fun"), SSymbol(name), SList(paramSorts.toList), returnSort)
    //case class DefineFun
    case Push(n) => {
      writer.write("(push ")
      writer.write(n.toString)
      writer.write(")\n")
    }
    case Pop(n) => {
      writer.write("(pop ")
      writer.write(n.toString)
      writer.write(")\n")
    }
    case Assert(term) => {
      writer.write("(assert ")
      printTerm(term, writer)
      writer.write(")\n")
    }
    case CheckSat() => {
      writer.write("(check-sat)\n")
    }
    case GetAssertions() => {
      writer.write("(get-assertions)\n")
    }
    case GetProof() => {
     writer.write("(get-proof)\n")
    }
    case GetUnsatCore() => {
      writer.write("(get-unsat-core)\n")
    }
    case GetValue(t, ts) => {
      writer.write("(get-value ")
      ???
    }
    case GetAssignment() => {
      writer.write("(get-assignment)\n")
    }
    case GetOption(SKeyword(key)) => {
      writer.write("(get-option :")
      writer.write(key)
      writer.write(")\n")
    }
    case GetInfo(flag) => {
      writer.write("(get-info ")
      printInfoFlag(flag, writer)
      writer.write(")\n")
    }
    case Exit() => {
      writer.write("(exit)\n")
    }
    case NonStandardCommand(expr) => {
      ???
    }
  }

  def printTerm(term: Term, writer: Writer): Unit = ???

  private def printSort(sort: Sort, writer: Writer): Unit = ???

  private def printLogic(logic: Logic, writer: Writer): Unit = logic match {
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

  def printAttribute(attribute: Attribute, writer: Writer): Unit = {

  }

  def printOption(option: SMTOption, writer: Writer): Unit = option match {
    case PrintSuccess(value) => 
      writer.write(":print-success ")
      writer.write(value.toString)
    case ExpandDefinitions(value) => 
      writer.write(":expand-definitions ")
      writer.write(value.toString)
    case InteractiveMode(value) => 
      writer.write(":interactive-mode ")
      writer.write(value.toString)
    case ProduceProofs(value) => 
      writer.write(":produce-proofs ")
      writer.write(value.toString)
    case ProduceUnsatCores(value) => 
      writer.write(":produce-unsat-cores ")
      writer.write(value.toString)
    case ProduceModels(value) => 
      writer.write(":produce-models ")
      writer.write(value.toString)
    case ProduceAssignments(value) => 
      writer.write(":produce-assignments ")
      writer.write(value.toString)
    case RegularOutputChannel(value) => 
      writer.write(":regular-output-channel ")
      writer.write('"')
      writer.write(value)
      writer.write('"')
    case DiagnosticOutputChannel(value) => 
      writer.write(":diagnostic-output-channel ")
      writer.write('"')
      writer.write(value)
      writer.write('"')
    case RandomSeed(num) => 
      writer.write(":random-seed ")
      writer.write(num.toString)
    case Verbosity(num) => 
      writer.write(":verbosity ")
      writer.write(num.toString)
    case AttributeOption(attribute) => 
      printAttribute(attribute, writer)
  }

  def printInfoFlag(flag: InfoFlag, writer: Writer): Unit = flag match {
    case ErrorBehaviourInfoFlag => 
      writer.write(":error-behaviour")
    case NameInfoFlag => 
      writer.write(":name")
    case AuthorsInfoFlag => 
      writer.write(":author")
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

  //private def ppNary(writer: Writer, exprs: Seq[SExpr], pre: String, op: String, post: String): Unit = {
  //  writer.write(pre)
  //  var c = 0
  //  var sz = exprs.size

  //  exprs.foreach(e => {
  //    apply(e, writer)
  //    c += 1
  //    if(c < sz) writer.write(op)
  //  })
  //  writer.write(post)
  //}

}
