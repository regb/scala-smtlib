package smtlib

import Commands.Attribute

object CommandResponses {

  sealed trait CommandResponse

  sealed trait GenResponse extends CommandResponse
  case object Success extends GenResponse {
    override def toString = "success"
  }
  case object Unsupported extends GenResponse {
    override def toString = "unsupported"
  }
  case class Error(msg: String) extends GenResponse {
    override def toString = """(error "%s")""".format(msg)
  }

  sealed trait CheckSatResponse extends CommandResponse
  case object SatStatus extends CommandResponse {
    override def toString = "sat"
  }
  case object UnsatStatus extends CommandResponse {
    override def toString = "unsat"
  }
  case object UnknownStatus extends CommandResponse {
    override def toString = "unknown"
  }

  /*
   * Info Response stuff
   */
  sealed trait ErrorBehaviour
  case object ImmediateExitErrorBehaviour extends ErrorBehaviour {
    override def toString = "immediate-exit"
  }
  case object ContinuedExecutionErrorBehaviour extends ErrorBehaviour {
    override def toString = "continued-execution"
  }

  sealed trait ReasonUnknown
  case object TimeoutReasonUnknown extends ErrorBehaviour {
    override def toString = "timeout"
  }
  case object IncompleteReasonUnknown extends ErrorBehaviour {
    override def toString = "incomplete"
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
    override def toString = ":reason-unknown " + reason
  }
  case class AttributeInfoResponse(attribute: Attribute) extends InfoResponse {
    override def toString = attribute.toString
  }

}
