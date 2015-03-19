package smtlib
package parser

import parser.Terms._
import common._

object Commands {

  sealed abstract class Command extends Positioned with SExpr {
    override def toString: String = printer.RecursivePrinter.toString(this)
  }

  case class Script(commands: List[Command])

  case class SetLogic(logic: Logic) extends Command
  case class SetOption(option: SMTOption) extends Command
  case class SetInfo(attribute: Attribute) extends Command

  case class DeclareSort(name: SSymbol, arity: Int) extends Command
  case class DefineSort(name: SSymbol, params: Seq[SSymbol], sort: Sort) extends Command
  case class DeclareFun(name: SSymbol, paramSorts: Seq[Sort], returnSort: Sort) extends Command
  case class DefineFun(name: SSymbol, params: Seq[SortedVar], returnSort: Sort, body: Term) extends Command

  case class Push(n: Int) extends Command
  case class Pop(n: Int) extends Command
  case class Assert(term: Term) extends Command

  case class CheckSat() extends Command
  case class GetAssertions() extends Command
  case class GetProof() extends Command
  case class GetUnsatCore() extends Command
  case class GetValue(term: Term, terms: Seq[Term]) extends Command
  case class GetAssignment() extends Command

  case class GetOption(key: SKeyword) extends Command
  case class GetInfo(flag: InfoFlag) extends Command

  case class Exit() extends Command

  //this command can be used to create and print arbitrary commands using the s-expression
  //It can be used to send commands not supported in this library, such as non-standard commands like declare-datatypes
  case class NonStandardCommand(sexpr: SExpr) extends Command


  //z3 get-model
  case class GetModel() extends Command
  //non standard declare-datatypes (no support for parametric types)
  case class DeclareDatatypes(datatypes: Seq[(SSymbol, Seq[Constructor])]) extends Command

  case class Constructor(sym: SSymbol, fields: Seq[(SSymbol, Sort)])

  /* 
   * Info flags can be queried with get-info command and
   * the SMT solver should support the following set of standard
   * flags. Additional solver-specific flags are supported via the general
   * KeywordInfoFlag
   */
  sealed abstract class InfoFlag
  case object ErrorBehaviorInfoFlag extends InfoFlag
  case object NameInfoFlag extends InfoFlag
  case object AuthorsInfoFlag extends InfoFlag
  case object VersionInfoFlag extends InfoFlag
  case object StatusInfoFlag extends InfoFlag
  case object ReasonUnknownInfoFlag extends InfoFlag
  case object AllStatisticsInfoFlag extends InfoFlag
  case class KeywordInfoFlag(keyword: String) extends InfoFlag

  /*
   * Options that can be passed to the underlying SMT solver.
   * A bunch of standard option (defined by the SMT-LIB standard) and
   * a generic syntax via attribute allows for solver-specific options
   */
  sealed abstract class SMTOption
  case class PrintSuccess(value: Boolean) extends SMTOption
  case class ExpandDefinitions(value: Boolean) extends SMTOption
  case class InteractiveMode(value: Boolean) extends SMTOption
  case class ProduceProofs(value: Boolean) extends SMTOption
  case class ProduceUnsatCores(value: Boolean) extends SMTOption
  case class ProduceModels(value: Boolean) extends SMTOption
  case class ProduceAssignments(value: Boolean) extends SMTOption
  case class RegularOutputChannel(value: String) extends SMTOption
  case class DiagnosticOutputChannel(value: String) extends SMTOption
  case class RandomSeed(value: Int) extends SMTOption
  case class Verbosity(value: Int) extends SMTOption
  case class AttributeOption(attribute: Attribute) extends SMTOption


  /*
   * TODO: It would probably be more consistent to use a SSymbol as 
   *       the logic in SetLogic command, and maybe provides a list of
   *       extractor for standard logics instead of a separate type.
   *       The reason being that the standard only requires a symbol here.
   */

  trait Logic 

  trait StandardLogic extends Logic

  case object AUFLIA extends StandardLogic
  case object AUFLIRA extends StandardLogic
  case object AUFNIRA extends StandardLogic
  case object LRA extends StandardLogic
  case object QF_ABV extends StandardLogic
  case object QF_AUFBV extends StandardLogic
  case object QF_AUFLIA extends StandardLogic
  case object QF_AX extends StandardLogic
  case object QF_BV extends StandardLogic
  case object QF_IDL extends StandardLogic
  case object QF_LIA extends StandardLogic
  case object QF_LRA extends StandardLogic
  case object QF_NIA extends StandardLogic
  case object QF_NRA extends StandardLogic
  case object QF_RDL extends StandardLogic
  case object QF_UF extends StandardLogic
  case object QF_UFBV extends StandardLogic
  case object QF_UFIDL extends StandardLogic
  case object QF_UFLIA extends StandardLogic
  case object QF_UFLRA extends StandardLogic
  case object QF_UFNRA extends StandardLogic
  case object UFLRA extends StandardLogic
  case object UFNIA extends StandardLogic
  case class NonStandardLogic(sym: SSymbol) extends Logic

  object Logic {
    val standardLogicFromString: PartialFunction[String, StandardLogic] = {
      case "AUFLIA" => AUFLIA
      case "AUFLIRA" => AUFLIRA
      case "AUFNIRA" => AUFNIRA
      case "LRA" => LRA
      case "QF_ABV" => QF_ABV
      case "QF_AUFBV" => QF_AUFBV
      case "QF_AUFLIA" => QF_AUFLIA
      case "QF_AX" => QF_AX
      case "QF_BV" => QF_BV
      case "QF_IDL" => QF_IDL
      case "QF_LIA" => QF_LIA
      case "QF_LRA" => QF_LRA
      case "QF_NIA" => QF_NIA
      case "QF_NRA" => QF_NRA
      case "QF_RDL" => QF_RDL
      case "QF_UF" => QF_UF
      case "QF_UFBV" => QF_UFBV
      case "QF_UFIDL" => QF_UFIDL
      case "QF_UFLIA" => QF_UFLIA
      case "QF_UFLRA" => QF_UFLRA
      case "QF_UFNRA" => QF_UFNRA
      case "UFLRA" => UFLRA
      case "UFNIA" => UFNIA
    }
  }

}
