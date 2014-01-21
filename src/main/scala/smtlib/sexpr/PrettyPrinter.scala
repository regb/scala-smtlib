package smtlib.sexpr

import SExprs._

object PrettyPrinter {

  def apply(sexpr: SExpr): String = sexpr match {
    case SList(sexprs) => sexprs.map(PrettyPrinter.apply).mkString("(", " ", ")")
    case SString(s) => '"' + s + '"'
    case SSymbol(s) => s
    case SQualifiedSymbol(os, s) => os.map(apply).getOrElse("") + ":" + apply(s)
    case SInt(i) => i.toString
    case SDouble(d) => d.toString
    case SComment(s) => ";" + s + "\n"
  }

}
