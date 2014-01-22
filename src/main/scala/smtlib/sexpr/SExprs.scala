package smtlib.sexpr

object SExprs {

  sealed trait SExpr
  case class SList(sexprs: List[SExpr]) extends SExpr
  object SList {
    def apply(sexprs: SExpr*): SList = SList(List(sexprs:_*))
  }
  case class SInt(n: BigInt) extends SExpr
  case class SDouble(n: Double) extends SExpr
  case class SString(s: String) extends SExpr
  case class SSymbol(s: String) extends SExpr
  case class SQualifiedSymbol(q: Option[SSymbol], s: SSymbol) extends SExpr
  case class SComment(s: String) extends SExpr /* Never parsed, only used for pretty printing */


  //TODO: SBoolean ???
}
