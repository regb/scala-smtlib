package smtlib
package parser

import Commands._
import Terms._

object CommandsResponses {

  sealed abstract class CommandResponse {
    override def toString: String = printer.RecursivePrinter.toString(this)
  }

  /*
   * A response can either be a successful response,
   * or one of Unsupported and Error.
   *
   * We define all the different possible responses categories as sealed trait
   * from which Unsupported and Error inherit. Additionnally each of the
   * trait have a concrete successful response that provide the response content.
   */

  /*
   * GenResponse is returned by some command, it's a basic Success object
   * for the successful kind, or Unsupported/Error
   */
  sealed trait GenResponse extends CommandResponse
  sealed trait GetInfoResponse extends CommandResponse
  sealed trait GetOptionResponse extends CommandResponse
  sealed trait CheckSatResponse extends CommandResponse
  sealed trait GetAssertionsResponse extends CommandResponse
  sealed trait GetProofResponse extends CommandResponse
  sealed trait GetUnsatCoreResponse extends CommandResponse
  sealed trait GetValueResponse extends CommandResponse
  sealed trait GetAssignmentResponse extends CommandResponse
  sealed trait GetModelResponse extends CommandResponse


  sealed trait AllResponseKind extends GenResponse with GetInfoResponse 
                               with GetOptionResponse with CheckSatResponse 
                               with GetAssertionsResponse with GetProofResponse
                               with GetUnsatCoreResponse with GetValueResponse
                               with GetAssignmentResponse with GetModelResponse

  sealed trait SuccessfulResponse extends CommandResponse
  sealed trait FailureResponse extends CommandResponse with AllResponseKind

  case object Unsupported extends AllResponseKind with FailureResponse
  case class Error(msg: String) extends AllResponseKind with FailureResponse

  /*
   * All of the different successful response are of type SuccessfulResponse
   * As well as one of the response kind.
   */

  case object Success extends 
    GenResponse with SuccessfulResponse

  case class CheckSatStatus(status: Status) extends 
    CheckSatResponse with SuccessfulResponse

  case class GetInfoResponseSuccess(info: InfoResponse, infos: Seq[InfoResponse]) extends 
    GetInfoResponse with SuccessfulResponse

  case class GetOptionResponseSuccess(attributeValue: AttributeValue) extends 
    GetOptionResponse with SuccessfulResponse

  case class GetAssertionsResponseSuccess(assertions: Seq[Term]) extends 
    GetAssertionsResponse with SuccessfulResponse

  case class GetValueResponseSuccess(valuationPairs: Seq[(Term, Term)]) extends 
    GetValueResponse with SuccessfulResponse

  case class GetUnsatCoreResponseSuccess(symbols: Seq[SSymbol]) extends 
    GetUnsatCoreResponse with SuccessfulResponse

  case class GetAssignmentResponseSuccess(valuationPair: Seq[(SSymbol, Boolean)]) extends
    GetAssignmentResponse with SuccessfulResponse

  case class GetProofResponseSuccess(proof: SExpr) extends 
    GetProofResponse with SuccessfulResponse


  /*
   * Temporary solution to mark NonStandardResponse until
   * they become standard or we find a cleaner solution
   */
  sealed trait NonStandardResponse extends CommandResponse

  /*
   * Z3 get-model. the model is a list of SExpr, but most S-Expr are actually well structured
   * like define-fun commands. We use SExpr as there are some ForAll Term as well (which are
   * not the same type as Command)
   */
  case class GetModelResponseSuccess(model: List[SExpr]) 
    extends GetModelResponse with SuccessfulResponse with NonStandardResponse


  /*
   * Helper types
   */
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
}
