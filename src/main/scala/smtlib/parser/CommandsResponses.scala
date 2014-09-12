package smtlib
package parser

import Commands._
import Terms._

object CommandsResponses {

  sealed abstract class CommandResponse {
    override def toString: String = printer.PrettyPrinter.toString(this)
  }

  sealed trait GenResponse extends CommandResponse
  case object Success extends GenResponse
  case object Unsupported extends GenResponse
  case class Error(msg: String) extends GenResponse

  sealed trait Status
  case object SatStatus extends Status
  case object UnsatStatus extends Status
  case object UnknownStatus extends Status

  sealed trait ErrorBehaviour
  case object ImmediateExitErrorBehaviour extends ErrorBehaviour {
    override def toString = "immediate-exit"
  }
  case object ContinueExecutionErrorBehaviour extends ErrorBehaviour {
    override def toString = "continue-execution"
  }

  sealed trait ReasonUnknown
  case object TimeoutReasonUnknown extends ReasonUnknown {
    override def toString = "timeout"
  }
  case object MemoutReasonUnknown extends ReasonUnknown {
    override def toString = "memout"
  }
  case object IncompleteReasonUnknown extends ReasonUnknown {
    override def toString = "incomplete"
  }


  case class CheckSatResponse(status: Status) extends CommandResponse {
    override def toString = status.toString
  }

  case class GetInfoResponse(info: InfoResponse, infos: Seq[InfoResponse]) extends CommandResponse {
    override def toString = (info +: infos).mkString("(", " ", ")")
  }
  sealed trait InfoResponse
  case class ErrorBehaviourInfoResponse(errorBehaviour: ErrorBehaviour) extends InfoResponse {
    override def toString = ":error-behaviour " + errorBehaviour.toString
  }
  case class NameInfoResponse(name: String) extends InfoResponse {
    override def toString = ":name \"%s\"".format(name)
  }
  case class AuthorsInfoResponse(authors: String) extends InfoResponse {
    override def toString = ":authors \"%s\"".format(authors)
  }
  case class VersionInfoResponse(version: String) extends InfoResponse {
    override def toString = ":version \"%s\"".format(version)
  }
  case class ReasonUnkownionInfoResponse(reason: ReasonUnknown) extends InfoResponse {
    override def toString = ":reason-unknown " + reason.toString
  }
  //as far as I can tell, that one is not in the specification, although there is a corresponding get-info
  //case class StatusInfoResponse(status: Status) extends InfoResponse {
  //  override def toString = ":status " + status.toString
  //}
  case class AttributeInfoResponse(attribute: Attribute) extends InfoResponse {
    override def toString = attribute.toString
  }

  //TODO: attributeValue trait more precise than SExpr
  case class GetOptionResponse(attributeValue: SExpr) extends CommandResponse


  case class GetValueResponse(valuationPairs: Seq[(Term, Term)]) extends CommandResponse

  case class GetAssertionsResponse(assertions: Seq[Term]) extends CommandResponse

  case class GetUnsatCoreResponse(symbols: Seq[SSymbol]) extends CommandResponse

  case class GetAssignmentResponse(valuationPair: Seq[(SSymbol, Boolean)]) extends CommandResponse

  case class GetProofResponse(proof: SExpr) extends CommandResponse


  case class SExprResponse(sexpr: SExpr) extends CommandResponse

  sealed trait NonStandardResponse extends CommandResponse

  //Z3 get-model
  case class GetModelResponse(model: List[DefineFun]) extends NonStandardResponse


}
