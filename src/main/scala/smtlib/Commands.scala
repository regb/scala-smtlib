package smtlib

import sexpr.SExprs._

object Commands {

  //TODO: TraversableOnce vs LinearSeq ?
  case class Script(cmds: TraversableOnce[Command])

  sealed abstract class Command
  case class SetLogic(logic: Logic) extends Command
  case class SetOption(option: SMTOption) extends Command
  case class SetInfo(attribute: Attribute) extends Command
  case class DeclareSort(name: String, arity: Int) extends Command
  //case class DefineSort
  case class DeclareFun(name: String, paramSorts: Seq[SExpr], returnSort: SExpr) extends Command
  //case class DefineFun
  case class Push(n: Int) extends Command
  case class Pop(n: Int) extends Command
  case class Assert(term: SExpr) extends Command
  case object CheckSat extends Command
  case object GetAssertions extends Command
  case object GetProof extends Command
  case object GetUnsatCore extends Command
  case class GetValue(term: SExpr, terms: Seq[SExpr]) extends Command
  case object GetAssignment extends Command
  case class GetOption(key: String) extends Command
  case class GetInfo(flag: InfoFlag) extends Command
  case object Exit extends Command

  case class Attribute(name: String, v: Option[SExpr])

  /* 
   * Info flags can be queried with get-info command and
   * the SMT solver should support the following set of standard
   * flags. Additional solver-specific flags are supported via the general
   * KeywordInfoFlag
   */
  sealed trait InfoFlag
  case object ErrorBehaviourInfoFlag extends InfoFlag
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
  sealed trait SMTOption
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

  case class Identifier(symbol: SSymbol, ns: Seq[Int])
  //case class SymbolId(symbol: SSymbol) extends Identifier
  //case class IndexedId(symbol: SSymbol, n1: Int, ns: Seq[Int]) extends Identifier

  case class Sort(id: Identifier, subSorts: Seq[Sort])
  //case class SymbolSort(symbol: SSymbol) extends Sort
  //case class 

  sealed abstract trait Logic
  case object QF_UF extends Logic
  case object QF_LRA extends Logic
  case object QF_AX extends Logic
  case object QF_A extends Logic
  case object Undef extends Logic

  object Logic {
    def fromString(logic: String): Logic = logic match {
      case "QF_UF"  => QF_UF
      case "QF_LRA" => QF_LRA
      case "QF_AX"  => QF_AX
      case "QF_A"   => QF_A
    }
  }

  abstract sealed trait CommandResponse

  abstract sealed trait GenResponse extends CommandResponse
  case object Success extends GenResponse {
    override def toString = "success"
  }
  case object Unsupported extends GenResponse {
    override def toString = "unsupported"
  }
  case class Error(msg: String) extends GenResponse {
    override def toString = "(error \"" + msg + "\")"
  }

  abstract sealed trait CheckSatResponse extends CommandResponse
  case object SatStatus extends CommandResponse {
    override def toString = "sat"
  }
  case object UnsatStatus extends CommandResponse {
    override def toString = "unsat"
  }
  case object UnknownStatus extends CommandResponse {
    override def toString = "unknown"
  }

}
