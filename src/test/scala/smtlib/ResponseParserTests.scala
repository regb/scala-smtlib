package smtlib

import common._
import Commands._
import CommandResponses._
import sexpr.SExprs._
import Parser._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class ResponseParserTests extends FunSuite with Timeouts {

  override def suiteName = "SMT-LIB Commands Reponses Parser suite"

  test("basic responses") {
    val reader1 = new StringReader("success")
    val parser1 = new ResponseParser(reader1)
    assert(parser1.hasNext)
    assert(parser1.next === Success)
    assert(!parser1.hasNext)

    val reader2 = new StringReader("unsupported")
    val parser2 = new ResponseParser(reader2)
    assert(parser2.hasNext)
    assert(parser2.next === Unsupported)
    assert(!parser2.hasNext)

    val reader3 = new StringReader("sat")
    val parser3 = new ResponseParser(reader3)
    assert(parser3.hasNext)
    assert(parser3.next === CheckSatResponse(SatStatus))
    assert(!parser3.hasNext)
  }

}
