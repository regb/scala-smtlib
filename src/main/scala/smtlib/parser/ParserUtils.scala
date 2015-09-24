package smtlib
package parser

import lexer.Tokens
import Tokens.{Token, TokenKind}
import lexer.Lexer

import scala.collection.mutable.ListBuffer

trait ParserUtils {
  
  val lexer: Lexer

  import Parser._

  private var _currentToken: Token = null
  /* lookAhead token is Some(null) if we reached eof */
  private var _lookAhead: Option[Token] = None

  //return a next token or throw UnexpectedEOFException if the next token is null
  protected def nextToken(): Token = {
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
  protected def peekToken: Token = {
    _lookAhead match {
      case Some(t) => t
      case None => {
        _lookAhead = Some(lexer.nextToken)
        _lookAhead.get
      }
    }
  }

  /*
   * Same as peekToken, but expect a token to be present
   * and result in a UnexpectedEOFException if the token is null
   */
  protected def getPeekToken: Token = {
    val tmp = peekToken
    if(tmp == null)
      throw new UnexpectedEOFException(Seq())
    tmp
  }

  /*
   * Make sure the next token has the expected kind and read it
   */
  protected def eat(expected: TokenKind): Unit = {
    val token = nextToken
    check(token, expected)
  }
  /*
   * Make sure the next token is exactly ``expected`` (usually a symbol lit)
   */
  protected def eat(expected: Token): Unit = {
    val token = nextToken
    if(token != expected)
      throw new UnexpectedTokenException(token, Seq(expected.kind))
  }


  protected def check(current: Token, exp: TokenKind): Unit = {
    if(current == null)
      throw new UnexpectedEOFException(Seq(exp))

    if(current.kind != exp) {
      expected(current, exp)
    }
  }

  protected def parseUntil[A](endKind: TokenKind, eatEnd: Boolean = true)(parseFun: () => A): Seq[A] = {
    val items = new ListBuffer[A]
    while(peekToken != null && peekToken.kind != endKind)
      items.append(parseFun())

    if(eatEnd)
      eat(endKind)

    items.toList
  }

  protected def parseMany[A](parseFun: () => A): Seq[A] = {
    eat(Tokens.OParen)
    parseUntil(Tokens.CParen)(parseFun)
  }

  /* Parse a sequence of A inside () */
  protected def parseOneOrMore[A](parseFun: () => A): (A, Seq[A]) = {
    val items = new ListBuffer[A]
    eat(Tokens.OParen)
    val head = parseFun()
    while(peekToken != null && peekToken.kind != Tokens.CParen)
      items.append(parseFun())
    eat(Tokens.CParen)
    (head, items.toList)
  }

  //TODO: we need a token class/type, instead of precise token with content + position
  protected def expected(found: Token, expected: TokenKind*): Nothing = {
    if(found == null)
      throw new UnexpectedEOFException(expected)
    else
      throw new UnexpectedTokenException(found, expected)
  }

}
