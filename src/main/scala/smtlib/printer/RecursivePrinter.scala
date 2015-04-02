package smtlib
package printer

import parser.Commands._
import parser.CommandsResponses._
import parser.Terms._

import java.io.Writer
import java.io.StringWriter
import java.io.BufferedWriter

object RecursivePrinter extends Printer with TerminalTreesPrinter {

  override val name: String = "recursive-printer"

  override def printScript(script: Script, writer: Writer): Unit = {
    script.commands.foreach(cmd => {
      printCommand(cmd, writer)
      writer.write("\n")
    })
  }

  override def printCommand(command: Command, writer: Writer): Unit = command match {
    case Assert(term) => {
      writer.write("(assert ")
      printTerm(term, writer)
      writer.write(")\n")
    }
    case CheckSat() => {
      writer.write("(check-sat)\n")
    }
    case CheckSatAssuming(props) => {
      writer.write("(check-sat-assuming ")
      printNary(writer, props, printPropLit _, "(", " ", ")")
      writer.write(')')
    }

    case DeclareConst(name, sort) => {
      writer.write("(declare-const ")
      writer.write(name.name)
      writer.write(' ')
      printSort(sort, writer)
      writer.write(")\n")
    }
    case DeclareFun(name, paramSorts, returnSort) => {
      writer.write("(declare-fun ")
      writer.write(name.name)
      printNary(writer, paramSorts, printSort _, " (", " ", ") ")
      printSort(returnSort, writer)
      writer.write(")\n")
    }
    case DeclareSort(name, arity) => {
      writer.write("(declare-sort ")
      writer.write(name.name)
      writer.write(" ")
      writer.write(arity.toString)
      writer.write(")\n")
    }

    case DefineFun(FunDef(name, sortedVars, returnSort, body)) => {
      writer.write("(define-fun ")
      writer.write(name.name)
      printNary(writer, sortedVars, printSortedVar _, " (", " ", ") ")
      printSort(returnSort, writer)
      writer.write(" ")
      printTerm(body, writer)
      writer.write(")\n")
    }
    case DefineFunRec(FunDef(name, sortedVars, returnSort, body)) => {
      writer.write("(define-fun-rec ")
      writer.write(name.name)
      printNary(writer, sortedVars, printSortedVar _, " (", " ", ") ")
      printSort(returnSort, writer)
      writer.write(" ")
      printTerm(body, writer)
      writer.write(")\n")
    }
    case DefineFunsRec(funDecs, bodies) => {
      writer.write("(define-funs-rec ")
      printNary(writer, funDecs, printFunDec _, "(", " ", ")")
      printNary(writer, bodies, printTerm _, "(", " ", ")")
      writer.write(")\n")
    }
    case DefineSort(name, params, sort) => {
      writer.write("(define-sort ")
      writer.write(name.name)
      writer.write(params.map(_.name).mkString(" (", " ", ") "))
      printSort(sort, writer)
      writer.write(")\n")
    }

    case Echo(value) => {
      writer.write("(echo ")
      printConstant(value, writer)
      writer.write(')')
    }
    case Exit() => {
      writer.write("(exit)\n")
    }

    case GetAssertions() => {
      writer.write("(get-assertions)\n")
    }
    case GetAssignment() => {
      writer.write("(get-assignment)\n")
    }
    case GetInfo(flag) => {
      writer.write("(get-info ")
      printInfoFlag(flag, writer)
      writer.write(")\n")
    }
    case GetModel() => {
      writer.write("(get-model)\n")
    }
    case GetOption(SKeyword(key)) => {
      writer.write("(get-option :")
      writer.write(key)
      writer.write(")\n")
    }
    case GetProof() => {
      writer.write("(get-proof)\n")
    }
    case GetUnsatAssumptions() => {
      writer.write("(get-unsat-assumptions)\n")
    }
    case GetUnsatCore() => {
      writer.write("(get-unsat-core)\n")
    }
    case GetValue(t, ts) => {
      writer.write("(get-value ")
      printNary(writer, t +: ts, printTerm _, "(", " ", "))")
    }

    case Pop(n) => {
      writer.write("(pop ")
      writer.write(n.toString)
      writer.write(")\n")
    }
    case Push(n) => {
      writer.write("(push ")
      writer.write(n.toString)
      writer.write(")\n")
    }

    case Reset() => {
      writer.write("(reset)\n")
    }
    case ResetAssertions() => {
      writer.write("(reset-assertions)\n")
    }

    case SetInfo(attribute) => {
      writer.write("(set-info ")
      printAttribute(attribute, writer)
      writer.write(")\n")
    }
    case SetOption(option) => {
      writer.write("(set-option ")
      printOption(option, writer)
      writer.write(")\n")
    }
    case SetLogic(logic) => {
      writer.write("(set-logic ")
      printLogic(logic, writer)
      writer.write(")\n")
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

  override def printTerm(term: Term, writer: Writer): Unit = term match {
    case Let(vb, vbs, t) =>
      writer.write("(let (")
      printVarBinding(vb, writer)
      printNary(writer, vbs, printVarBinding _, "", " ", ") ")
      printTerm(t, writer)
      writer.write(")")
    case ForAll(sortedVar, sortedVars, t) =>
      writer.write("(forall (")
      printSortedVar(sortedVar, writer)
      printNary(writer, sortedVars, printSortedVar _, "", " ", ") ")
      printTerm(t, writer)
      writer.write(")")
    case Exists(sortedVar, sortedVars, t) =>
      writer.write("(exists (")
      printSortedVar(sortedVar, writer)
      printNary(writer, sortedVars, printSortedVar _, "", " ", ") ")
      printTerm(t, writer)
      writer.write(")")
    case FunctionApplication(fun, ts) =>
      if (ts.nonEmpty) {
        writer.write("(")
        printQualifiedId(fun, writer)
        printNary(writer, ts, printTerm _, " ", " ", ")")
      } else {
        printQualifiedId(fun, writer)
      }
    case AnnotatedTerm(term, attr, attrs) => {
      writer.write("(! ")
      printTerm(term, writer)
      printNary(writer, attr +: attrs, printAttribute _, " ", " ", ")")
    }
    case id@QualifiedIdentifier(_, _) => 
      printQualifiedId(id, writer)

    case (c: Constant) => printConstant(c, writer)
  }

  override def printSort(sort: Sort, writer: Writer): Unit = {
    val id = sort.id
    if(sort.subSorts.isEmpty)
      printId(id, writer)
    else {
      writer.write("(")
      printId(id, writer)
      printNary(writer, sort.subSorts, printSort _, " ", " ", ")")
    }
  }
  
  override def printCommandResponse(response: CommandResponse, writer: Writer): Unit = response match {
    case Success => 
      writer.write("success\n")
    case Unsupported =>
      writer.write("unsupported\n")
    case Error(msg) =>
      writer.write("(error ")
      writer.write('"')
      writer.write(msg)
      writer.write('"')
      writer.write(")\n")

    case CheckSatStatus(SatStatus) =>
      writer.write("sat\n")
    case CheckSatStatus(UnsatStatus) =>
      writer.write("unsat\n")
    case CheckSatStatus(UnknownStatus) =>
      writer.write("unknown\n")

    case EchoResponseSuccess(value) =>
      printString(value, writer)

    case GetAssertionsResponseSuccess(assertions) =>
      printNary(writer, assertions, printTerm, "(", " ", " )")

    case GetAssignmentResponseSuccess(valuationPairs) => {
      def printValuationPair(pair: (SSymbol, Boolean), writer: Writer): Unit = {
        writer.write('(')
        printSymbol(pair._1, writer)
        writer.write(' ')
        writer.write(pair._2.toString)
        writer.write(')')
      }
      printNary(writer, valuationPairs, printValuationPair, "(", " ", ")")
    }

    case GetInfoResponseSuccess(response, responses) =>
      printNary(writer, response +: responses, printInfoResponse, "(", " ", ")")

    case GetModelResponseSuccess(exprs) => {
      def printGetModelResponseEntry(expr: SExpr, writer: Writer): Unit = expr match {
        case (cmd: Command) => printCommand(cmd, writer)
        case (term: Term) => printTerm(term, writer)
        case _ => printSExpr(expr, writer)
      }
      writer.write("(model \n")
      printNary(writer, exprs, printGetModelResponseEntry, "", "\n", "")
      writer.write(')')
    }

    case GetOptionResponseSuccess(av) =>
      printSExpr(av, writer)

    case GetProofResponseSuccess(proof) =>
      printSExpr(proof, writer)

    case GetUnsatAssumptionsResponseSuccess(symbols) =>
      printNary(writer, symbols, printSExpr, "(", " ", ")")

    case GetUnsatCoreResponseSuccess(symbols) =>
      printNary(writer, symbols, printSExpr, "(", " ", ")")

    case GetValueResponseSuccess(valuationPairs) => {
      def printValuationPair(pair: (Term, Term), writer: Writer): Unit = {
        writer.write('(')
        printTerm(pair._1, writer)
        writer.write(' ')
        printTerm(pair._2, writer)
        writer.write(')')
      }
      printNary(writer, valuationPairs, printValuationPair, "(", " ", ")")
    }
  }


  private def printOption(option: SMTOption, writer: Writer): Unit = option match {
    case DiagnosticOutputChannel(value) => 
      writer.write(":diagnostic-output-channel ")
      writer.write('"')
      writer.write(value)
      writer.write('"')

    case ExpandDefinitions(value) => 
      writer.write(":expand-definitions ")
      writer.write(value.toString)
    case GlobalDeclarations(value) => 
      writer.write(":global-declarations ")
      writer.write(value.toString)

    case InteractiveMode(value) => 
      writer.write(":interactive-mode ")
      writer.write(value.toString)
    case PrintSuccess(value) => 
      writer.write(":print-success ")
      writer.write(value.toString)

    case ProduceAssertions(value) => 
      writer.write(":produce-assertions ")
      writer.write(value.toString)
    case ProduceAssignments(value) => 
      writer.write(":produce-assignments ")
      writer.write(value.toString)
    case ProduceProofs(value) => 
      writer.write(":produce-proofs ")
      writer.write(value.toString)
    case ProduceModels(value) => 
      writer.write(":produce-models ")
      writer.write(value.toString)
    case ProduceUnsatAssumptions(value) => 
      writer.write(":produce-unsat-assumptions ")
      writer.write(value.toString)
    case ProduceUnsatCores(value) => 
      writer.write(":produce-unsat-cores ")
      writer.write(value.toString)

    case RegularOutputChannel(value) => 
      writer.write(":regular-output-channel ")
      writer.write('"')
      writer.write(value)
      writer.write('"')

    case RandomSeed(num) => 
      writer.write(":random-seed ")
      writer.write(num.toString)

    case ReproducibleResourceLimit(num) => 
      writer.write(":reproducible-resource-limit ")
      writer.write(num.toString)
    case Verbosity(num) => 
      writer.write(":verbosity ")
      writer.write(num.toString)

    case AttributeOption(attribute) => 
      printAttribute(attribute, writer)
  }

  private def printInfoResponse(infoResponse: InfoResponse, writer: Writer): Unit = infoResponse match {
    case AssertionStackLevelsInfoResponse(level) =>
      writer.write(":assertion-stack-levels ")
      writer.write(level.toString)
    case AuthorsInfoResponse(authors) =>
      writer.write(":authors \"")
      writer.write(authors)
      writer.write('"')
    case ErrorBehaviorInfoResponse(ImmediateExitErrorBehavior) =>
      writer.write(":error-behavior immediate-exit")
    case ErrorBehaviorInfoResponse(ContinuedExecutionErrorBehavior) =>
      writer.write(":error-behavior continued-execution")
    case NameInfoResponse(name) =>
      writer.write(":name \"")
      writer.write(name)
      writer.write('"')
    case ReasonUnknownInfoResponse(TimeoutReasonUnknown) =>
      writer.write(":reason-unknown timeout")
    case ReasonUnknownInfoResponse(MemoutReasonUnknown) =>
      writer.write(":reason-unknown memout")
    case ReasonUnknownInfoResponse(IncompleteReasonUnknown) =>
      writer.write(":reason-unknown incomplete")
    case VersionInfoResponse(version) =>
      writer.write(":version \"")
      writer.write(version)
      writer.write('"')
    case AttributeInfoResponse(attribute) =>
      printAttribute(attribute, writer)
  }

  private def printAttribute(attribute: Attribute, writer: Writer): Unit = {
    printKeyword(attribute.keyword, writer)
    attribute.value match {
      case Some(value) =>
        writer.write(" ")
        printSExpr(value, writer) //all attribute values are s-exprs
      case None => ()
    }
  }

  private def printSExpr(sexpr: SExpr, writer: Writer): Unit = sexpr match {
    case SList(es) =>
      printNary(writer, es, printSExpr _, "(", " ", ")")
    case SKeyword(key) =>
      writer.write(":")
      writer.write(key)
    case s@SSymbol(_) =>
      printSymbol(s, writer)
    case (c: Constant) =>
      printConstant(c, writer)
  }


  private def printQualifiedId(qi: QualifiedIdentifier, writer: Writer): Unit = qi.sort match {
    case None =>
      printId(qi.id, writer)
    case Some(sort) =>
      writer.write("(as ")
      printId(qi.id, writer)
      writer.write(" ")
      printSort(sort, writer)
      writer.write(")")
  }

  private def printVarBinding(vb: VarBinding, writer: Writer): Unit = {
    writer.write('(')
    writer.write(vb.name.name)
    writer.write(' ')
    printTerm(vb.term, writer)
    writer.write(')')
  }

  private def printSortedVar(sv: SortedVar, writer: Writer): Unit = {
    writer.write('(')
    writer.write(sv.name.name)
    writer.write(' ')
    printSort(sv.sort, writer)
    writer.write(')')
  }

  protected def printFunDec(funDec: FunDec, writer: Writer): Unit = {
    writer.write('(')
    printSymbol(funDec.name, writer)
    printNary(writer, funDec.params, printSortedVar _, " (", " ", ") ")
    printSort(funDec.returnSort, writer)
    writer.write(')')
  }

  private def printNary[A](
    writer: Writer, as: Seq[A], printer: (A, Writer) => Unit,
    pre: String, op: String, post: String): Unit = {

    writer.write(pre)

    var c = 0
    var sz = as.size

    as.foreach(a => {
      printer(a, writer)
      c += 1
      if(c < sz) writer.write(op)
    })
    writer.write(post)
  }

}
