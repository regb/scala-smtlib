package smtlib
package lexer

import common._


object Tokens {

  sealed trait Token extends Positioned

  /*
   * Need to be case class since each instance is different because of their positions
   * Alternatively, we could create a Token wrapper class that would be Positioned and
   * use the list of Tokens as a token information element.
   */
  case class OParen() extends Token /* ( */
  case class CParen() extends Token /* ) */

  case class StringLit(content: String) extends Token /* "hello" */

  case class SymbolLit(content: String) extends Token /* hello */
  case class QualifiedSymbol(qualifier: Option[String], name: String) extends Token /* foo:bar */

  case class NumeralLit(n: BigInt) extends Token /* 42 */
  case class DecimalLit(d: Double) extends Token /* 42.24 */ //TODO: infinite precision ?
  case class BinaryLit(content: Seq[Boolean]) extends Token /* #b0101 */ 
  case class HexadecimalLit(content: String) extends Token /* #xFF1D */ {
    require(content.forall(c =>
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    ))
  }

}
