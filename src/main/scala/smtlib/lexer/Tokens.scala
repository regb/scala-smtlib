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
  case class Keyword(name: String) extends Token /* :bar */

  case class NumeralLit(n: BigInt) extends Token /* 42 */
  case class DecimalLit(d: Double) extends Token /* 42.24 */ //TODO: infinite precision ?
  case class BinaryLit(content: Seq[Boolean]) extends Token /* #b0101 */ 
  case class HexadecimalLit(content: String) extends Token /* #xFF1D */ {
    //should be normalized to upper cases
    require(content.forall(c =>
      (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')
    ))
  }

  sealed trait ReservedWord extends Token
  case class Par() extends ReservedWord
  case class NUMERAL() extends ReservedWord
  case class DECIMAL() extends ReservedWord
  case class STRING() extends ReservedWord
  case class Underscore() extends ReservedWord /* _ */
  case class ExclamationMark() extends ReservedWord /* ! */
  case class As() extends ReservedWord /* as */
  case class Let() extends ReservedWord /* let */
  case class Forall() extends ReservedWord /* forall */
  case class Exists() extends ReservedWord /* exists */

  //TODO: all commands are reserved words as well


}
