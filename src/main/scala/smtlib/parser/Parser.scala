package smtlib
package parser

import lexer.Tokens
import Tokens.Token
import lexer.Lexer

import Terms._
import Commands._

import common._


class Parser(lexer: Lexer) {

  import Parser._

  private var _currentToken: Token = null
  /* lookAhead token is Some(null) if we reached eof */
  private var _lookAhead: Option[Token] = None

  //return a next token or null if EOF
  private def nextToken: Token = {
    _lookAhead match {
      case Some(t) => {
        _lookAhead = None
        _currentToken = t
        t
      }
      case None => {
        _currentToken = lexer.nextToken
        _lookAhead = None
        _currentToken
      }
    }
  }

  /*
   * return the look ahead token, or null if EOF
   * Note: do not throw an exception as it is okay to lookahead into EOF
   */
  private def peekToken: Token = {
    _lookAhead match {
      case Some(t) => t
      case None => {
        _lookAhead = Some(lexer.nextToken)
        _lookAhead.get
      }
    }
  }

  /*
   * Make sure the next token corresponds to t and read
   */
  private def eat(expected: Token): Unit = {
    val token = nextToken
    check(token, expected)
  }

  private def check(current: Token, expected: Token): Unit = {
    if(current != expected) {
      sys.error("unexpected token")
    }
  }


  def parse: Script = {
    ???
  }

  def parseCommand: Command = {
    val head = nextToken
    check(head, Tokens.OParen())

    val cmd = nextToken match {
      case Tokens.SetLogic() => {
        val logicSymbol: SSymbol = parseSymbol
        val logic = Logic.fromString(logicSymbol.name)
        SetLogic(logic)
      }

      case Tokens.Assert() => ???

      case _ => sys.error("TODO")
    }
    eat(Tokens.CParen())

    cmd.setPos(head)
  }

  def parseSymbol: SSymbol = {
    nextToken match {
      case t@Tokens.SymbolLit(s) => {
        val symbol = SSymbol(s)
        symbol.setPos(t)
      }
      case _ => sys.error("TODO")
    }
  }


}

//object Parser {
//
//  class EOFBeforeMatchingParenthesisException(startPos: Position) extends
//    Exception("Opened parenthesis at position: " + startPos + " has no matching closing parenthesis")
//  class UnexpectedTokenException(token: Token, pos: Position) extends
//    Exception("Unexpected token: " + token + " at position: " + pos)
//
//  def fromString(str: String): Parser = {
//    val lexer = new Lexer(new java.io.StringReader(str))
//    new Parser(lexer)
//  }
//
//  def fromReader(reader: java.io.Reader): Parser = {
//    val lexer = new Lexer(reader)
//    new Parser(lexer)
//  }
//
//  /*
//   * Parse a string and return the next SExpr in the string, ignore the rest
//   */
//  def exprFromString(str: String): SExpr = {
//    val lexer = new Lexer(new java.io.StringReader(str))
//    val parser = new Parser(lexer)
//    parser.next
//  }
//}
//
//// vim: set ts=4 sw=4 et:
