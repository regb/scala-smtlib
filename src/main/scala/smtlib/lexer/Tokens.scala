package smtlib
package lexer

import common._

object Tokens {

  sealed class Token(val kind: TokenKind) extends Positioned {
    override def toString = kind.toString
  }
  object Token {
    def apply(kind: TokenKind): Token = new Token(kind)
    def unapply(token: Token): Option[TokenKind] = Some(token.kind)
  }

  case class StringLit(content: String) extends Token(StringLitKind)
  case class SymbolLit(content: String) extends Token(SymbolLitKind)
  case class Keyword(name: String) extends Token(KeywordKind)

  case class NumeralLit(n: BigInt) extends Token(NumeralLitKind)
  case class DecimalLit(d: Double) extends Token(DecimalLitKind)
  case class BinaryLit(content: Seq[Boolean]) extends Token(BinaryLitKind)
  case class HexadecimalLit(content: Hexadecimal) extends Token(HexadecimalLitKind)

  sealed abstract class TokenKind

  case object OParen extends TokenKind /* ( */
  case object CParen extends TokenKind /* ) */

  case object StringLitKind extends TokenKind /* "hello" */
  case object SymbolLitKind extends TokenKind /* hello */
  case object KeywordKind extends TokenKind /* :bar */
  case object NumeralLitKind extends TokenKind /* 42 */
  case object DecimalLitKind extends TokenKind /* 42.24 */
  case object BinaryLitKind extends TokenKind /* #b0101 */ 
  case object HexadecimalLitKind extends TokenKind /* #xFF1D */

  sealed trait ReservedWord extends TokenKind
  case object Par extends ReservedWord
  case object NUMERAL extends ReservedWord
  case object DECIMAL extends ReservedWord
  case object STRING extends ReservedWord
  case object Underscore extends ReservedWord /* _ */
  case object ExclamationMark extends ReservedWord /* ! */
  case object As extends ReservedWord /* as */
  case object Let extends ReservedWord /* let */
  case object ForAll extends ReservedWord /* forall */
  case object Exists extends ReservedWord /* exists */

  case object Assert extends ReservedWord
  case object CheckSat extends ReservedWord
  case object DeclareSort extends ReservedWord
  case object DeclareFun extends ReservedWord
  case object DefineSort extends ReservedWord
  case object DefineFun extends ReservedWord
  case object Exit extends ReservedWord
  case object GetAssertions extends ReservedWord
  case object GetAssignment extends ReservedWord
  case object GetInfo extends ReservedWord
  case object GetOption extends ReservedWord
  case object GetProof extends ReservedWord
  case object GetUnsatCore extends ReservedWord
  case object GetValue extends ReservedWord
  case object Pop extends ReservedWord
  case object Push extends ReservedWord
  case object SetLogic extends ReservedWord
  case object SetInfo extends ReservedWord
  case object SetOption extends ReservedWord

  case object DeclareDatatypes extends ReservedWord

}
