package smtlib

import Commands._

import sexpr.SExprs._

object PrettyPrinter {

  def apply(command: Command): String = ???
  
  def cmdToSExpr(cmd: Command): SExpr = cmd match {
    case SetLogic(logic) => SList(SSymbol("set-logic"), logicToSExpr(logic))
    case SetOption(option) => SList(SSymbol("set-option"), optionToSExpr(option))
    case SetInfo(attribute) => SList(SSymbol("set-info"), attributeToSExpr(attribute))
    //case class DeclareSort(name: SSymbol, arity: Int) extends Command
    ////case class DefineSort
    //case class DeclareFun(name: SSymbol, paramSorts: Seq[SExpr], returnSort: SExpr) extends Command
    ////case class DefineFun
    //case class Assert(term: SExpr) extends Command
    //case class Push(n: Int) extends Command
    //case class Pop(n: Int) extends Command
    //case object CheckSat extends Command
    ////case object GetValue extends Command
    //case object GetProof extends Command
    //case object GetUnsatCore extends Command
    //case object GetAssertions extends Command
    //case object GetAssignment extends Command
    //case class GetOption(key: String) extends Command
    case GetInfo(flag) => SList(SSymbol("get-info"), infoFlagToSExpr(flag))
    case Exit => SList(SSymbol("exit"))
    case _ => ???
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
    case NameInfoFlag => SQualifiedSymbol(None, SSymbol("error-behavious"))
    case NameInfoFlag => SQualifiedSymbol(None, SSymbol("name"))
    case AuthorsInfoFlag => SQualifiedSymbol(None, SSymbol("author"))
    case VersionInfoFlag => SQualifiedSymbol(None, SSymbol("version"))
    case StatusInfoFlag => SQualifiedSymbol(None, SSymbol("status"))
    case ReasonUnknownInfoFlag => SQualifiedSymbol(None, SSymbol("reason-unknown"))
    case AllStatisticsInfoFlag => SQualifiedSymbol(None, SSymbol("all-statistics"))
    case KeywordInfoFlag(keyword) => SQualifiedSymbol(None, SSymbol(keyword))
  }

}
