package smtlib.sexpr

import SExprs._
import java.io.StringReader

import org.scalatest.FunSuite

class PrettyPrinterTests extends FunSuite {

  import PrettyPrinter.{toString => pp}

  private def checkExpr(expr: SExpr): Unit = {
    assert(pp(expr) === pp(Parser.exprFromString(pp(expr))))
  }

  /* Seems legit to assume a literal should be printed without extra white space,
   * although one could argue. However we shall not test symbols as they have 
   * many representations (even confusing some of the best SMT solvers in the world)
   */
  test("literals") {
    assert(pp(SInt(42)) === "42")
    assert(pp(SDouble(13.42)) === "13.42")
    assert(pp(SString("sexprs")) === "\"sexprs\"")
  }

  test("simple exprs") {
    checkExpr(SList(SSymbol("add"), SInt(23), SSymbol("xyz")))
    checkExpr(SList(SSymbol("ADD"), SList(SSymbol("min"), SInt(12), SInt(42)), SDouble(22.11)))
  }

  test("composed exprs") {
    checkExpr(SList(
      SList(SSymbol("add"), SInt(23), SSymbol("xyz")),
      SList(SInt(42), SDouble(12.2))
    ))
  }

}
