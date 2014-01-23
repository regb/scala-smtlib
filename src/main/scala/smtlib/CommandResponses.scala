package smtlib

object CommandResponses {

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
