package smtlib

import sexpr.SExprs._
import CommandResponses._

import scala.collection.Iterator

object ResponseParser {
  class UnknownCommandResponse(msg: String) extends Exception(msg)

}

class ResponseParser(input: java.io.Reader) extends Iterator[CommandResponse] {

  import ResponseParser._

  private val l = new sexpr.Lexer(input)
  private val p = new sexpr.Parser(l)

  private var lookAhead: Option[SExpr] = None

  override def hasNext: Boolean = {
    lookAhead match {
      case Some(expr) => expr != null
      case None => {
        val c = p.parse
        lookAhead = Some(c)
        c != null
      }
    }
  }

  override def next: CommandResponse = {
    val response = lookAhead match {
      case None => p.parse
      case Some(c) => {
        lookAhead = None
        c
      }
    }
    if(response == null)
      throw new NoSuchElementException
    val res = response match {
      case SSymbol("success") => Success
      case SSymbol("unsuported") => Unsupported
      case SList(List(SSymbol("error"), SString(msg))) => Error(msg)
      case SSymbol("sat") | SSymbol("SAT") => CheckSatResponse(SatStatus)
      case SSymbol("unsat") | SSymbol("UNSAT") => CheckSatResponse(UnsatStatus)
      case SSymbol("unknown") | SSymbol("UNKNOWN") => CheckSatResponse(UnknownStatus)
      case SList(List(SSymbol(""), SString(msg))) => Error(msg)
      case sexpr => SExprResponse(sexpr)
    }
    res
  }
}
