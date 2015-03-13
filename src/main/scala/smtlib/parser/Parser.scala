package smtlib
package parser

import lexer.Tokens
import Tokens.{Token, TokenKind}
import lexer.Lexer

import Terms._
import Commands._
import CommandsResponses._

import common._

import scala.collection.mutable.ListBuffer

class Parser(val lexer: Lexer) extends ParserUtils with ParserTerms with ParserCommands with ParserCommandsResponses

object Parser {

  class UnknownCommandException(val commandName: TokenKind) extends Exception("Unknown command name token: " + commandName)

  class UnexpectedTokenException(found: Token, expected: Seq[TokenKind])
    extends Exception("Unexpected token at position: " + found.getPos + ". Expected: " + expected.mkString("[",",","]") + ". Found: " + found)

  class UnexpectedEOFException(expected: Seq[TokenKind])
    extends Exception("Unexpected end of file. Expected: " + expected.mkString("[",",","]"))

  def fromString(str: String): Parser = {
    val lexer = new Lexer(new java.io.StringReader(str))
    new Parser(lexer)
  }

}
