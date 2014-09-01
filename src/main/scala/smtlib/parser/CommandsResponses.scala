package smtlib
package parser

import Commands._
import Terms._

object CommandsResponses {

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

  sealed trait Status
  case object SatStatus extends Status {
    override def toString = "sat"
  }
  case object UnsatStatus extends Status {
    override def toString = "unsat"
  }
  case object UnknownStatus extends Status {
    override def toString = "unknown"
  }

  sealed trait ErrorBehaviour
  case object ImmediateExitErrorBehaviour extends ErrorBehaviour {
    override def toString = "immediate-exit"
  }
  case object ContinueExecutionErrorBehaviour extends ErrorBehaviour {
    override def toString = "continue-execution"
  }

  sealed trait ReasonUnknown
  case object TimeoutReasonUnknown extends ErrorBehaviour {
    override def toString = "timeout"
  }
  case object IncompleteReasonUnknown extends ErrorBehaviour {
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
  case class StatusInfoResponse(status: Status) extends InfoResponse {
    override def toString = ":status " + status.toString
  }
  case class AttributeInfoResponse(attribute: Attribute) extends InfoResponse {
    override def toString = attribute.toString
  }

  //TODO: attributeValue trait more precise than SExpr
  case class GetOptionResponse(attributeValue: SExpr) extends CommandResponse

  case class SExprResponse(sexpr: SExpr) extends CommandResponse

  object GetValueResponse {
    def apply(values: Seq[(SExpr, SExpr)]): CommandResponse = SExprResponse(SList(values.map(p => SList(p._1, p._2)).toList))
    def unapply(cmdRes: CommandResponse): Option[Seq[(SExpr, SExpr)]] = cmdRes match {
      case SExprResponse(SList(values)) => {
        scala.util.Try(values.map{
          case SList(List(s, v)) => (s, v)
          case _ => throw new Exception
        }).toOption
      }
      case _ => None
    }
  }

  sealed trait NonStandardResponse extends CommandResponse

  //Z3 get-model
  case class GetModelResponse(model: List[DefineFun]) extends NonStandardResponse


}
