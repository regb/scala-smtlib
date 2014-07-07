package smtlib
package sexpr

import common._
import SExprs._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class ParserTests extends FunSuite with Timeouts {

  test("parser basic") {
    val reader1 = new StringReader("""
      (test "test")
    """)
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    assert(parser1.next === SList(List(SSymbol("test"), SString("test"))))


    val reader2 = new StringReader("""
      (  42  42.173 )
    """)
    val lexer2 = new Lexer(reader2)
    val parser2 = new Parser(lexer2)
    assert(parser2.next === SList(List(SInt(42), SDouble(42.173))))

    val reader3 = new StringReader("""
      (  42  ("test1" 21))
    """)
    val lexer3 = new Lexer(reader3)
    val parser3 = new Parser(lexer3)
    assert(parser3.next === SList(List(
      SInt(42), 
      SList(List(SString("test1"),SInt(21))))))

    val reader4 = new StringReader("""
      ()
    """)
    val lexer4 = new Lexer(reader4)
    val parser4 = new Parser(lexer4)
    assert(parser4.next === SList(List()))
  }

  test("packages") {
    val reader1 = new StringReader("""
      (:test "test")
    """)
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    assert(parser1.next === SList(List(SQualifiedSymbol(None, SSymbol("test")), SString("test"))))


    val reader2 = new StringReader("""
      (foo:bar :toto)
    """)
    val lexer2 = new Lexer(reader2)
    val parser2 = new Parser(lexer2)
    assert(parser2.next === 
           SList(List(
             SQualifiedSymbol(Some(SSymbol("foo")), SSymbol("bar")),
             SQualifiedSymbol(None, SSymbol("toto")))))

  }

  test("interactive parser") {
    val pis = new SynchronousPipedReader
    val lexer = failAfter(3 seconds) { new Lexer(pis) }
    val parser = new Parser(lexer)

    pis.write("(test 12)")
    assert(parser.next ===
      SList(List(SSymbol("test"), SInt(12))))

    pis.write("(+ 1 3)")
    assert(parser.next ===
      SList(List(SSymbol("+"), SInt(1), SInt(3))))
  }


  //TODO: testing exceptions and error handling

}
