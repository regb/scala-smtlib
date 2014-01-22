package smtlib.sexpr

import Tokens._
import SExprs._

class Parser(lexer: Lexer) {

  private var _currentToken: Token = null
  private var lookAhead: Option[Token] = None

  private def next: Token = {
    lookAhead match {
      case Some(t) => {
        lookAhead = None
        _currentToken = t
        t
      }
      case None => {
        _currentToken = lexer.next
        lookAhead = None
        _currentToken
      }
    }
  }

  private def peek: Token = {
    lookAhead match {
      case Some(t) => t
      case None =>
        lookAhead = Some(lexer.next)
        lookAhead.get
    }
  }

  private def eat(t: Token): Unit = {
    val c = next
    assert(c == t)
  }

  /* 
     Return the next SExpr if there is one, or null if EOF.
     Throw an EOFException if EOF is reached at an unexpected moment (incomplete SExpr).
  */
  def parse: SExpr = {
    val tok = next
    if(tok == null) null else tok match {
      case OParen => {
        val buffer = new scala.collection.mutable.ListBuffer[SExpr]
        while(peek != CParen) {
          buffer.append(parse)
        }
        eat(CParen)
        SList(buffer.toList)
        //var future = next
        //while(peek != CParen) {
        //  val child: SExpr = parse
        //  buffer.append(child)
        //}
        //eat(CParen)
        //SList(buffer.toList)
      }
      case IntLit(d) => SInt(d)
      case StringLit(s) => SString(s)
      case SymbolLit(s) => SSymbol(s)
      case QualifiedSymbol(o, s) => SQualifiedSymbol(o.map(SSymbol), SSymbol(s))
      case DoubleLit(d) => SDouble(d)
      case CParen => sys.error("Unexpected token: " + CParen)
    }
  }


}

object Parser {

  /*
   * Parse a string and return the next SExpr in the string, ignore the rest
   */
  def fromString(str: String): SExpr = {
    val lexer = new Lexer(new java.io.StringReader(str))
    val parser = new Parser(lexer)
    parser.parse
  }

}
