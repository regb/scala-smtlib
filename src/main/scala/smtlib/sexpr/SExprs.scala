package smtlib
package sexpr

import common.Positioned

object SExprs {

  sealed trait SExpr extends Positioned
  case class SList(sexprs: List[SExpr]) extends SExpr
  object SList {
    def apply(sexprs: SExpr*): SList = SList(List(sexprs:_*))
  }
  case class SInt(n: BigInt) extends SExpr
  case class SDouble(n: Double) extends SExpr
  case class SString(s: String) extends SExpr
  case class SBoolean(v: Boolean) extends SExpr
  case class SSymbol(s: String) extends SExpr
  case class SQualifiedSymbol(q: Option[SSymbol], s: SSymbol) extends SExpr

  /* SComment is never parsed, only used for pretty printing */
  case class SComment(s: String) extends SExpr 


}
