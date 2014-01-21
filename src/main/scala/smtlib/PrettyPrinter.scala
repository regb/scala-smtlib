package smtlib

import Commands._

import sexpr.SExprs._

object PrettyPrinter {

  def apply(command: Command): String = ???
  
  def cmdToSExpr(cmd: Command): SExpr = cmd match {
    case SetLogic(logic) => SList(SSymbol("set-logic"), logicToSExpr(logic))
    case SetOption(option) => SList(SSymbol("set-option"), optionToSExpr(option))
    //case class SetInfo(attribute: Attribute) extends Command
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
    //case class GetInfo(flag: InfoFlag) extends Command
    //case class GetOption(key: String) extends Command
    case Exit => SList(SSymbol("exit"))
    case _ => ???
  }

  def logicToSExpr(logic: Logic): SExpr = ???

  def attributeToSExpr(attribute: Attribute): SExpr = ???
  //final case class Attribute(name: String, v: Option[SExpr])

  def optionToSExpr(option: SMTOption): SExpr = ???
  //sealed trait SMTOption
  //case class PrintSuccess(value: Boolean) extends SMTOption
  //case class ExpandDefinitions(value: Boolean) extends SMTOption
  //case class InteractiveMode(value: Boolean) extends SMTOption
  //case class ProduceProofs(value: Boolean) extends SMTOption
  //case class ProduceUnsatCores(value: Boolean) extends SMTOption
  //case class ProduceModels(value: Boolean) extends SMTOption
  //case class ProduceAssignments(value: Boolean) extends SMTOption
  //case class RegularOutputChannel(value: String) extends SMTOption
  //case class DiagnosticOutputChannel(value: String) extends SMTOption
  //case class RandomSeed(value: Int) extends SMTOption
  //case class Verbosity(value: Int) extends SMTOption
  //case class AttributeOption(name: String, v: Option[SExpr]) extends SMTOption


  //sealed trait InfoFlag
  //case object NameInfoFlag extends InfoFlag
  //case object AuthorsInfoFlag extends InfoFlag
  //case object VersionInfoFlag extends InfoFlag
  //case object StatusInfoFlag extends InfoFlag
  //case object ReasonUnknownInfoFlag extends InfoFlag
  //case object AllStatisticsInfoFlag extends InfoFlag
  //case class KeywordInfoFlag(keyword: String) extends InfoFlag

}
