package smtlib

import common._
import Commands._
import sexpr.SExprs._
import Parser._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class ParserTests extends FunSuite with Timeouts {

  test("parser basic commands") {
    val reader1 = new StringReader("""(set-logic QF_UF)""")
    val parser1 = new Parser(reader1)
    assert(parser1.hasNext)
    assert(parser1.next === SetLogic(QF_UF))
    assert(!parser1.hasNext)

    val reader2 = new StringReader("""(exit)""")
    val parser2 = new Parser(reader2)
    assert(parser2.hasNext)
    assert(parser2.next === Exit)
    assert(!parser2.hasNext)

    val reader3 = new StringReader("""(check-sat)""")
    val parser3 = new Parser(reader3)
    assert(parser3.hasNext)
    assert(parser3.next === CheckSat)
    assert(!parser3.hasNext)
  }

  test("Unknown command") {
    val reader1 = new StringReader("""(alpha beta)""")
    val parser1 = new Parser(reader1)
    assert(parser1.hasNext)
    intercept[UnknownCommandException] {
      parser1.next
    }
  }

  test("simple benchmark") {
    val reader1 = new StringReader("""
      (set-logic QF_UF)
      (declare-fun f (Int) Int)
      (declare-fun a () Int)
      (assert (= (f a) a))
      (check-sat)
    """)
    val parser1 = new Parser(reader1)
    assert(parser1.hasNext)
    assert(parser1.next === SetLogic(QF_UF))
    assert(parser1.next === DeclareFun("f", Seq(SSymbol("Int")), SSymbol("Int")))
    assert(parser1.next === DeclareFun("a", Seq(), SSymbol("Int")))
    assert(parser1.next === Assert(SList(
                              SSymbol("="), SList(SSymbol("f"), SSymbol("a")), SSymbol("a"))))
    assert(parser1.next === CheckSat)
    assert(!parser1.hasNext)

  }

  test("interactive parser") {
    val pis = new SynchronousPipedReader
    val parser = failAfter(3 seconds) { new Parser(pis) }

    pis.write("(set-logic QF_LRA)")
    assert(parser.hasNext)
    assert(parser.next === SetLogic(QF_LRA))

    pis.write("(assert (< 1 3))")
    assert(parser.hasNext)
    assert(parser.next === Assert(SList(SSymbol("<"), SInt(1), SInt(3))))
  }


  //TODO: testing exceptions and error handling

}
