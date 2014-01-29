package smtlib

import Commands._

import sexpr.SExprs._

import java.io.Writer
import java.io.StringWriter

object PrettyPrinter {

  def apply(command: Command, writer: Writer): Unit = sexpr.PrettyPrinter(cmdToSExpr(command), writer)

  def apply(command: Command): String = {
    val sWriter = new StringWriter
    apply(command, sWriter)
    sWriter.toString
  }
  
  def cmdToSExpr(cmd: Command): SExpr = cmd match {
    case SetLogic(logic) => SList(SSymbol("set-logic"), logicToSExpr(logic))
    case SetOption(option) => SList(SSymbol("set-option"), optionToSExpr(option))
    case SetInfo(attribute) => SList(SSymbol("set-info"), attributeToSExpr(attribute))
    case DeclareSort(name, arity) => SList(SSymbol("declare-sort"), SSymbol(name), SInt(arity))
    //case class DefineSort
    case DeclareFun(name, paramSorts, returnSort) => 
      SList(SSymbol("declare-fun"), SSymbol(name), SList(paramSorts.toList), returnSort)
    //case class DefineFun
    case Push(n: Int) => SList(SSymbol("push"), SInt(n))
    case Pop(n: Int) => SList(SSymbol("pop"), SInt(n))
    case Assert(term) => SList(SSymbol("assert"), term)
    case CheckSat => SList(SSymbol("check-sat"))
    case GetAssertions => SList(SSymbol("get-assertions"))
    case GetProof => SList(SSymbol("get-proof"))
    case GetUnsatCore => SList(SSymbol("get-unsat-core"))
    case GetValue(t, ts) => SList(SSymbol("get-value"), SList((t +: ts).toList))
    case GetAssignment => SList(SSymbol("get-assignment"))
    case GetOption(key) => SList(SSymbol("get-option"), SQualifiedSymbol(None, SSymbol(key)))
    case GetInfo(flag) => SList(SSymbol("get-info"), infoFlagToSExpr(flag))
    case Exit => SList(SSymbol("exit"))
    case NonStandardCommand(expr) => expr
  }

  def logicToSExpr(logic: Logic): SExpr = ???

  def attributeToSExpr(attribute: Attribute): SExpr = ???
  //final case class Attribute(name: String, v: Option[SExpr])

  def optionToSExpr(option: SMTOption): SExpr = option match {
    case PrintSuccess(value) => 
      SList(SQualifiedSymbol(None, SSymbol("print-success")), SSymbol(value.toString))
    case ExpandDefinitions(value) =>
      SList(SQualifiedSymbol(None, SSymbol("expand-definitions")), SSymbol(value.toString))
    case InteractiveMode(value) =>
      SList(SQualifiedSymbol(None, SSymbol("interactive-mode")), SSymbol(value.toString))
    case ProduceProofs(value) =>
      SList(SQualifiedSymbol(None, SSymbol("produce-proofs")), SSymbol(value.toString))
    case ProduceUnsatCores(value) =>
      SList(SQualifiedSymbol(None, SSymbol("produce-unsat-cores")), SSymbol(value.toString))
    case ProduceModels(value) =>
      SList(SQualifiedSymbol(None, SSymbol("produce-models")), SSymbol(value.toString))
    case ProduceAssignments(value) =>
      SList(SQualifiedSymbol(None, SSymbol("produce-assignments")), SSymbol(value.toString))
    case RegularOutputChannel(value) =>
      SList(SQualifiedSymbol(None, SSymbol("regular-output-channel")), SString(value))
    case DiagnosticOutputChannel(value) =>
      SList(SQualifiedSymbol(None, SSymbol("diagnostic-output-channel")), SString(value))
    case RandomSeed(numeral) =>
      SList(SQualifiedSymbol(None, SSymbol("random-seed")), SInt(numeral))
    case Verbosity(numeral) =>
      SList(SQualifiedSymbol(None, SSymbol("verbosity")), SInt(numeral))
    case AttributeOption(attribute) => attributeToSExpr(attribute)
  }

  def infoFlagToSExpr(flag: InfoFlag): SExpr = flag match {
    case ErrorBehaviourInfoFlag => SQualifiedSymbol(None, SSymbol("error-behavious"))
    case NameInfoFlag => SQualifiedSymbol(None, SSymbol("name"))
    case AuthorsInfoFlag => SQualifiedSymbol(None, SSymbol("author"))
    case VersionInfoFlag => SQualifiedSymbol(None, SSymbol("version"))
    case StatusInfoFlag => SQualifiedSymbol(None, SSymbol("status"))
    case ReasonUnknownInfoFlag => SQualifiedSymbol(None, SSymbol("reason-unknown"))
    case AllStatisticsInfoFlag => SQualifiedSymbol(None, SSymbol("all-statistics"))
    case KeywordInfoFlag(keyword) => SQualifiedSymbol(None, SSymbol(keyword))
  }

}
