package smtlib
package parser

import Commands._
import Terms._

object CommandsResponses {

  sealed abstract class CommandResponse {
    override def toString: String = printer.RecursivePrinter.toString(this)
  }

  sealed trait GenResponse extends CommandResponse
  case object Success extends GenResponse
  case object Unsupported extends GenResponse
  case class Error(msg: String) extends GenResponse

  sealed trait Status
  case object SatStatus extends Status
  case object UnsatStatus extends Status
  case object UnknownStatus extends Status

  sealed trait ErrorBehavior
  case object ImmediateExitErrorBehavior extends ErrorBehavior
  case object ContinuedExecutionErrorBehavior extends ErrorBehavior

  sealed trait ReasonUnknown
  case object TimeoutReasonUnknown extends ReasonUnknown
  case object MemoutReasonUnknown extends ReasonUnknown
  case object IncompleteReasonUnknown extends ReasonUnknown


  case class CheckSatResponse(status: Status) extends CommandResponse

  case class GetInfoResponse(info: InfoResponse, infos: Seq[InfoResponse]) extends CommandResponse
  sealed trait InfoResponse
  case class ErrorBehaviorInfoResponse(errorBehavior: ErrorBehavior) extends InfoResponse
  case class NameInfoResponse(name: String) extends InfoResponse
  case class AuthorsInfoResponse(authors: String) extends InfoResponse
  case class VersionInfoResponse(version: String) extends InfoResponse
  case class ReasonUnknownInfoResponse(reason: ReasonUnknown) extends InfoResponse
  //as far as I can tell, that one is not in the specification, although there is a corresponding get-info
  //case class StatusInfoResponse(status: Status) extends InfoResponse {
  //  override def toString = ":status " + status.toString
  //}
  case class AttributeInfoResponse(attribute: Attribute) extends InfoResponse

  //TODO: attributeValue trait more precise than SExpr
  case class GetOptionResponse(attributeValue: SExpr) extends CommandResponse


  case class GetValueResponse(valuationPairs: Seq[(Term, Term)]) extends CommandResponse

  case class GetAssertionsResponse(assertions: Seq[Term]) extends CommandResponse

  case class GetUnsatCoreResponse(symbols: Seq[SSymbol]) extends CommandResponse

  case class GetAssignmentResponse(valuationPair: Seq[(SSymbol, Boolean)]) extends CommandResponse

  case class GetProofResponse(proof: SExpr) extends CommandResponse


  case class SExprResponse(sexpr: SExpr) extends CommandResponse

  sealed trait NonStandardResponse extends CommandResponse

  /*
   * Z3 get-model. the model is a list of SExpr, but most S-Expr are actually well structured
   * like define-fun commands. We use SExpr as there are some ForAll Term as well (which are
   * not the same type as Command)
   */
  case class GetModelResponse(model: List[SExpr]) extends NonStandardResponse


}
