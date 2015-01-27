package smtlib
package printer

import common.LinkedList

import parser.Commands._
import parser.CommandsResponses._
import parser.Terms._

import java.io.Writer
import java.io.StringWriter
import java.io.BufferedWriter

object TailPrinter {

  private type Action = () => Unit

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


  def printScript(script: Script, writer: Writer): Unit = {
    script.commands.foreach(cmd => {
      printCommand(cmd, writer)
      writer.write("\n")
    })
  }

  def printCommand(command: Command, writer: Writer): Unit = {
    val actionsBuffer = new LinkedList[Action]
    printCommand(command, writer, actionsBuffer)
    doActions(actionsBuffer)
  }
  def printCommand(command: Command, writer: Writer, actions: LinkedList[Action]): Unit = command match {
    case SetLogic(logic) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printLogic(logic, writer))
      actions.prepend(() => writer.write("(set-logic "))
    }
    case SetOption(option) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printOption(option, writer))
      actions.prepend(() => writer.write("(set-option "))
    }
    case SetInfo(attribute) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printAttribute(attribute, writer))
      actions.prepend(() => writer.write("(set-info "))
    }
    case DeclareSort(name, arity) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => writer.write(arity.toString))
      actions.prepend(() => writer.write(" "))
      actions.prepend(() => writer.write(name.name))
      actions.prepend(() => writer.write("(declare-sort "))
    }
    case DefineSort(name, params, sort) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printSort(sort, writer, actions))
      actions.prepend(() => writer.write(params.map(_.name).mkString(" (", " ", ") ")))
      actions.prepend(() => writer.write(name.name))
      actions.prepend(() => writer.write("(define-sort "))
    }
    case DeclareFun(name, paramSorts, returnSort) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printSort(returnSort, writer, actions))
      actions.prepend(() => printNary(writer, paramSorts, (s: Sort, writer: Writer) => printSort(s, writer, actions), " (", " ", ") ", actions))
      actions.prepend(() => writer.write(name.name))
      actions.prepend(() => writer.write("(declare-fun "))
    }
    case DefineFun(name, sortedVars, returnSort, body) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printTerm(body, writer, actions))
      actions.prepend(() => writer.write(" "))
      actions.prepend(() => printSort(returnSort, writer, actions))
      actions.prepend(() => printNary(writer, sortedVars, (v: SortedVar, w: Writer) => printSortedVar(v, w, actions), " (", " ", ") ", actions))
      actions.prepend(() => writer.write(name.name))
      actions.prepend(() => writer.write("(define-fun "))
    }
    case Push(n) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => writer.write(n.toString))
      actions.prepend(() => writer.write("(push "))
    }
    case Pop(n) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => writer.write(n.toString))
      actions.prepend(() => writer.write("(pop "))
    }
    case Assert(term) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printTerm(term, writer, actions))
      actions.prepend(() => writer.write("(assert "))
    }
    case CheckSat() => {
      actions.prepend(() => writer.write("(check-sat)\n"))
    }
    case GetAssertions() => {
      actions.prepend(() => writer.write("(get-assertions)\n"))
    }
    case GetProof() => {
     actions.prepend(() => writer.write("(get-proof)\n"))
    }
    case GetUnsatCore() => {
      actions.prepend(() => writer.write("(get-unsat-core)\n"))
    }
    case GetValue(t, ts) => {
      actions.prepend(() => printNary(writer, t +: ts, (t: Term, w: Writer) => printTerm(t, w, actions), "(", " ", "))", actions))
      actions.prepend(() => writer.write("(get-value "))
    }
    case GetAssignment() => {
      actions.prepend(() => writer.write("(get-assignment)\n"))
    }
    case GetOption(SKeyword(key)) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => writer.write(key))
      actions.prepend(() => writer.write("(get-option :"))
    }
    case GetInfo(flag) => {
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => printInfoFlag(flag, writer))
      actions.prepend(() => writer.write("(get-info "))
    }
    case Exit() => {
      actions.prepend(() => writer.write("(exit)\n"))
    }
    case NonStandardCommand(expr) => {
      actions.prepend(() => printSExpr(expr, writer, actions))
    }
    case GetModel() => {
      actions.prepend(() => writer.write("(get-model)\n"))
    }
    case DeclareDatatypes(datatypes) => {
      writer.write("(declare-datatypes () (")

      datatypes.foreach{ case (name, constructors) => {
        writer.write(" (")
        writer.write(name.name)
        constructors.foreach{ 
          case Constructor(sym, Seq()) => {
            writer.write(" (")
            writer.write(sym.name)
            writer.write(")")
          }
          case Constructor(sym, fields) => {
            writer.write(" (")
            writer.write(sym.name)
            fields.foreach{ case (field, sort) => {
              writer.write(" (")
              writer.write(field.name)
              writer.write(" ")
              printSort(sort, writer)
              writer.write(")")
            }}
            writer.write(")")
          }
        }
        writer.write(") ")
      }}

      writer.write("))\n")

    }
  }


  private def doActions(actions: LinkedList[Action]): Unit = {
    while(!actions.isEmpty) {
      val action = actions.pop()
      action()
    }
  }

  def printTerm(term: Term, writer: Writer): Unit = {
    val actionsBuffer = new LinkedList[Action]
    printTerm(term, writer, actionsBuffer)
    doActions(actionsBuffer)
  }

  def printTerm(term: Term, writer: Writer, actions: LinkedList[Action]): Unit = term match {
    case Let(vb, vbs, t) =>
      actions.prepend(() => writer.write(")"))
      actions.prepend(() => printTerm(t, writer, actions))
      actions.prepend(() => printNary(writer, vbs, (v: VarBinding, w: Writer) => printVarBinding(v, w, actions), "", " ", ") ", actions))
      actions.prepend(() => printVarBinding(vb, writer, actions))
      actions.prepend(() => writer.write("(let ("))
      
    case ForAll(sortedVar, sortedVars, t) =>
      actions.prepend(() => writer.write(")"))
      actions.prepend(() => printTerm(t, writer, actions))
      actions.prepend(() => printNary(writer, sortedVars, (v: SortedVar, w: Writer) => printSortedVar(v, w, actions), "", " ", ") ", actions))
      actions.prepend(() => printSortedVar(sortedVar, writer, actions))
      actions.prepend(() => writer.write("(forall ("))

    case Exists(sortedVar, sortedVars, t) =>
      actions.prepend(() => writer.write(")"))
      actions.prepend(() => printTerm(t, writer, actions))
      actions.prepend(() => printNary(writer, sortedVars, (v: SortedVar, w: Writer) => printSortedVar(v, w, actions), "", " ", ") ", actions))
      actions.prepend(() => printSortedVar(sortedVar, writer, actions))
      actions.prepend(() => writer.write("(exists ("))

    case FunctionApplication(fun, ts) =>
      if (ts.nonEmpty) {
        actions.prepend(() => printNary(writer, ts, printTerm _, " ", " ", ")", actions))
        actions.prepend(() => printQualifiedId(fun, writer, actions))
        actions.prepend(() => writer.write("("))
      } else {
        actions.prepend(() => printQualifiedId(fun, writer, actions))
      }

    case AnnotatedTerm(term, attr, attrs) => {
      actions.prepend(() => printNary(writer, attr +: attrs, printAttribute _, " ", " ", ")", actions))
      actions.prepend(() => printTerm(term, writer, actions))
      actions.prepend(() => writer.write("(! "))
    }
    case id@QualifiedIdentifier(_, _) => 
      actions.prepend(() => printQualifiedId(id, writer, actions))

    case (c: Constant) => 
      actions.prepend(() => printConstant(c, writer))
  }

  def printConstant(c: Constant, writer: Writer): Unit = c match {
    case SNumeral(value) => writer.write(value.toString)
    case SHexadecimal(value) => writer.write(value.toString)
    case SBinary(value) => writer.write("#b" + value.map(if(_) "1" else "0").mkString)
    case SDecimal(value) => writer.write(value.toString)
    case SString(value) =>
      writer.write("\"")
      writer.write(value.flatMap(c => if(c == '"') "\\\"" else List(c)))
      writer.write("\"")
  }

  private def printQualifiedId(qi: QualifiedIdentifier, writer: Writer, actions: LinkedList[Action]): Unit = qi.sort match {
    case None =>
      actions.prepend(() => printId(qi.id, writer))
    case Some(sort) =>
      actions.prepend(() => writer.write(")"))
      actions.prepend(() => printSort(sort, writer, actions))
      actions.prepend(() => writer.write(" "))
      actions.prepend(() => printId(qi.id, writer))
      actions.prepend(() => writer.write("(as "))
  }


  def printId(id: Identifier, writer: Writer): Unit = {
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

  private def printVarBinding(vb: VarBinding, writer: Writer, actions: LinkedList[Action]): Unit = {
    actions.prepend(() => writer.write(')'))
    actions.prepend(() => printTerm(vb.term, writer, actions))
    actions.prepend(() => writer.write(' '))
    actions.prepend(() => writer.write(vb.name.name))
    actions.prepend(() => writer.write('('))
  }

  private def printSortedVar(sv: SortedVar, writer: Writer, actions: LinkedList[Action]): Unit = {
    actions.prepend(() => writer.write(')'))
    actions.prepend(() => printSort(sv.sort, writer, actions))
    actions.prepend(() => writer.write(' '))
    actions.prepend(() => writer.write(sv.name.name))
    actions.prepend(() => writer.write('('))
  }

  def printSort(sort: Sort, writer: Writer): Unit = {
    val actionsBuffer = new LinkedList[Action]
    printSort(sort, writer, actionsBuffer)
    doActions(actionsBuffer)
  }
  private def printSort(sort: Sort, writer: Writer, actions: LinkedList[Action]): Unit = {
    val id = sort.id
    if(sort.subSorts.isEmpty)
      actions.prepend(() => printId(id, writer))
    else {
      actions.prepend(() => printNary(writer, sort.subSorts, (s: Sort, w: Writer) => printSort(s, w, actions), " ", " ", ")", actions))
      actions.prepend(() => printId(id, writer))
      actions.prepend(() => writer.write("("))
    }
  }

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
    printKeyword(attribute.keyword, writer)
    attribute.v match {
      case Some(sexpr) => 
        writer.write(" ")
        printSExpr(sexpr, writer)
      case None => ()
    }
  }

  def printKeyword(keyword: SKeyword, writer: Writer): Unit = {
    writer.write(":")
    writer.write(keyword.name)
  }


  def printSExpr(sexpr: SExpr, writer: Writer): Unit = {
    val actionsBuffer = new LinkedList[Action]
    printSExpr(sexpr, writer, actionsBuffer)
    doActions(actionsBuffer)
  }
  def printSExpr(sexpr: SExpr, writer: Writer, actions: LinkedList[Action]): Unit = sexpr match {
    case SList(es) =>
      printNary(writer, es, printSExpr _, "(", " ", ")", actions)
    case SKeyword(key) =>
      writer.write(":")
      writer.write(key)
    case SSymbol(sym) =>
      writer.write(sym)
    case _ => sys.error("TODO")
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

  
  def printCommandResponse(response: CommandResponse, writer: Writer): Unit = {
    val actionsBuffer = new LinkedList[Action]
    printCommandResponse(response, writer, actionsBuffer)
    doActions(actionsBuffer)
  }
  def printCommandResponse(response: CommandResponse, writer: Writer, actions: LinkedList[Action]): Unit = response match {
    case Success => 
      actions.prepend(() => writer.write("success\n"))
    case Unsupported =>
      actions.prepend(() => writer.write("unsupported\n"))
    case Error(msg) =>
      actions.prepend(() => writer.write(")\n"))
      actions.prepend(() => writer.write('"'))
      actions.prepend(() => writer.write(msg))
      actions.prepend(() => writer.write('"'))
      actions.prepend(() => writer.write("(error "))
    case CheckSatResponse(SatStatus) =>
      actions.prepend(() => writer.write("sat\n"))
    case CheckSatResponse(UnsatStatus) =>
      actions.prepend(() => writer.write("unsat\n"))
    case CheckSatResponse(UnknownStatus) =>
      actions.prepend(() => writer.write("unknown\n"))

    case GetValueResponse(valuationPairs) =>
      def printValuationPair(pair: (Term, Term), writer: Writer): Unit = {
        actions.prepend(() => writer.write(')'))
        actions.prepend(() => printTerm(pair._2, writer, actions))
        actions.prepend(() => writer.write(' '))
        actions.prepend(() => printTerm(pair._1, writer, actions))
        actions.prepend(() => writer.write('('))
      }
      printNary(writer, valuationPairs, printValuationPair, "(", " ", ")", actions)
    case _ => ???
  }

  private def printNary[A](
    writer: Writer, as: Seq[A], printer: (A, Writer) => Unit,
    pre: String, op: String, post: String, actions: LinkedList[Action]): Unit = {

    var newActions = new scala.collection.mutable.ListBuffer[Action]()

    newActions.append(() => writer.write(pre))

    var c = 0
    var sz = as.size

    as.foreach(a => {
      newActions.append(() => printer(a, writer))
      c += 1
      if(c < sz) 
        newActions.append(() => writer.write(op))
    })
    newActions.append(() => writer.write(post))

    newActions.toList.reverse.foreach(action =>
      actions.prepend(action)
    )
  }

}
